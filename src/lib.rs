use std::collections::HashMap;
use std::slice::Iter;
use std::str::FromStr;

pub struct CommandLine<'a> {
    commands: HashMap<&'a str, Command<'a>>,
}

impl<'a> CommandLine<'a> {
    pub fn new() -> Self {
        Self {
            commands: HashMap::new(),
        }
   }

    pub fn register(&mut self, command: Command<'a>) -> &mut Self {
        let name_copy = command.name.clone();
        
        self.commands.insert(command.name, command);
        CommandLine::check_subcommands_ids(self.commands.get_mut(name_copy).unwrap(), false);
        
        self
    }

    fn check_subcommands_ids(command: &'a mut Command, from_sub: bool) {
        const PARENT_ID_DELIMITER: char = '.';

        for sub in command.subcommands.iter_mut() {
            if from_sub {
                sub.id.insert_str(0, &command.id);
                sub.id.insert(command.id.len(), PARENT_ID_DELIMITER);
            }
            
            if !sub.subcommands.is_empty() {
                CommandLine::check_subcommands_ids(sub, true);
            }
        }
    }

    pub fn deregister(&mut self, command: Command<'a>) {
        self.commands.remove(command.name);
    }

    pub fn lookup(&self, name: &str) -> Option<&Command<'a>> {
        self.commands.get(name)
    }

    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Command<'a>> {
        self.commands.get_mut(name)
    }

    pub fn run(&mut self, input: &str) -> Result<ParsedCommand, ParseError> {
        parsing::parse(self, input)        
    }
}

pub enum ParameterVal {
    Text(String),
    I32(i32),
    U32(u32),
    List(Vec<Parameter>),
}

pub struct Parameter {
    pub val: ParameterVal,
}

impl std::fmt::Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.val {
            ParameterVal::Text(s) => write!(f, "{}", s),
            ParameterVal::I32(n) => write!(f, "{}", n),
            ParameterVal::U32(n) => write!(f, "{}", n),
            ParameterVal::List(l) => {
                let mut res = String::with_capacity(l.len() * 3);
                let mut i = 1;
                res.push('[');

                for e in l {
                    res.push_str(&e.to_string());
                    i += 1;
                    if i != l.len() {
                        res.push(',');
                        res.push(' ');
                    }
                }

                res.push(']');
                write!(f, "{}", res)
            }
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum ParameterKind {
    Text,
    I32,
    U32,
    List(Box<ParameterKind>),
    None,
}

const PARAM_KIND_TEXT_STR: &str = "text";
const PARAM_KIND_I32_STR: &str = "i32";
const PARAM_KIND_U32_STR: &str = "u32";
const PARAM_KIND_LIST_STR: &str = "list";
const PARAM_KIND_NONE_STR: &str = "none";

impl std::fmt::Display for ParameterKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let name = match self {
            ParameterKind::Text => PARAM_KIND_TEXT_STR,
            ParameterKind::I32 => PARAM_KIND_I32_STR,
            ParameterKind::U32 => PARAM_KIND_U32_STR,
            ParameterKind::List(_) => PARAM_KIND_LIST_STR,
            ParameterKind::None => PARAM_KIND_NONE_STR,
        };

        f.write_str(name)
    }
}

impl FromStr for ParameterKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            PARAM_KIND_TEXT_STR => Ok(ParameterKind::Text),
            PARAM_KIND_I32_STR => Ok(ParameterKind::I32),
            PARAM_KIND_U32_STR => Ok(ParameterKind::U32),
            _ => {
                if s.starts_with(PARAM_KIND_LIST_STR) {
                    let mut raw_element_type = String::new();
                    let mut started_type = false;

                    for c in s.chars() {
                        match c {
                            '(' => started_type = true,
                            ')' => break,
                            _ => {
                                if started_type {
                                    raw_element_type.push(c);
                                }
                            }
                        }
                    }

                    if let Some(e) = ParameterKind::from_str(raw_element_type.as_str()).ok() {
                        return Ok(ParameterKind::List(Box::new(e)));
                    }
                }

                return Err(());
            }
        }
    }
}

#[derive(Clone)]
pub struct OptionData {
    pub short_flags: Vec<char>,
    pub long_flags: Vec<String>,
    pub param_kind: ParameterKind,
}

pub struct Opt {
    data: OptionData,
    parameter: Option<Parameter>,
}

impl Opt {
    pub fn parameter(&self) -> &Parameter {
        self.parameter.as_ref().unwrap()
    }
}

pub enum ParseError {
    UnknownCommand,
    MissingSubcommand,
    MissingParameter(ParameterKind),
    InvalidParameter(String),
    UnnecessaryFlag(String),
    UnnecessaryParameter(String),
    InvalidSyntax(usize),
}

pub struct Command<'a> {
    aliases: Vec<&'a str>,
    id: String,
    name: &'a str,
    subcommands: Vec<Command<'a>>,
    //branches_need_sort: bool,
    param_types: Vec<ParameterKind>,
    options: Vec<OptionData>,
}

impl<'a> Command<'a> {
    pub fn new(aliases: Vec<&'a str>) -> Self {
        let aliases_tmp = aliases.clone();
        let name = aliases_tmp
            .get(0)
            .filter(|v| !v.is_empty())
            .expect("at least one alias must be specified");

        Self {
            aliases,
            name,
            id: name.to_string(),
            subcommands: Vec::new(),
            param_types: Vec::new(),
            options: Vec::new(),
        }
    }

    pub fn add_subcommand(mut self, subcommand: Command<'a>) -> Self {
        self.subcommands.push(subcommand);
        
        self
    }

    pub fn set_syntax_format(mut self, format: &str) -> Self {
        let (mut param_types, mut options) = parsing::parse_params_and_options(format).unwrap_or_else(|e| panic!(e));

        self.param_types.append(&mut param_types);
        self.options.append(&mut options);

        self
    }
}

pub trait ParamValueShortcut {
    fn as_str(&self) -> &str;

    fn as_i32(&self) -> i32;

    fn as_u32(&self) -> u32;

    fn as_multi(&self) -> &[Parameter];
}

impl ParamValueShortcut for Parameter {
    fn as_str(&self) -> &str {
        if let ParameterVal::Text(v) = &self.val {
            return v.as_str();
        } else {
            panic!()
        }
    }

    fn as_i32(&self) -> i32 {
        if let ParameterVal::I32(v) = self.val {
            return v;
        } else {
            panic!()
        }
    }

    fn as_u32(&self) -> u32 {
        if let ParameterVal::U32(v) = self.val {
            return v;
        } else {
            panic!()
        }
    }

    fn as_multi(&self) -> &[Parameter] {
        if let ParameterVal::List(v) = &self.val {
            return v;
        } else {
            panic!()
        }
    }
}

pub trait OptionAccessor {
    fn by_short_flag(&self, flag: char) -> Option<&Opt>;

    fn by_long_flag(&self, flag: &str) -> Option<&Opt>;
}

impl OptionAccessor for Vec<Opt> {
    fn by_short_flag(&self, flag: char) -> Option<&Opt> {
        self.iter().find(|f| f.data.short_flags.contains(&flag))
    }

    fn by_long_flag(&self, flag: &str) -> Option<&Opt> {
        self.iter().find(|f| f.data.long_flags.contains(&flag.to_owned()))
    }
}

pub trait ParamAccessor {
    fn poll(&mut self) -> &Parameter;

    fn poll_str(&mut self) -> &str;

    fn poll_i32(&mut self) -> i32;

    fn poll_multi(&mut self) -> &[Parameter];

    fn poll_multi_str(&mut self) -> Vec<&str>;

    fn poll_multi_i32(&mut self) -> Vec<i32>;
}

impl<'a> ParamAccessor for Iter<'a, Parameter> {
    fn poll(&mut self) -> &Parameter {
        self.next().unwrap()
    }

    fn poll_str(&mut self) -> &str {
        self.poll().as_str()
    }

    fn poll_i32(&mut self) -> i32 {
        self.poll().as_i32()
    }

    fn poll_multi(&mut self) -> &[Parameter] {
        self.poll().as_multi()
    }

    fn poll_multi_str(&mut self) -> Vec<&str> {
        self.poll_multi().iter().map(|p| p.as_str()).collect()
    }

    fn poll_multi_i32(&mut self) -> Vec<i32> {
        self.poll_multi().iter().map(|p| p.as_i32()).collect()
    }
}

pub struct ParsedCommand<'a> {
    pub name: &'a str,
    pub subcommand_id: &'a str, // Option<&'a str>,
    pub parameters: Vec<Parameter>,
    pub options: Vec<Opt>,
}

mod parsing {
    use super::*;

    pub fn parse<'b, 'a: 'b>(
        command_line: &'b mut CommandLine<'a>,
        input: &str,
    ) -> Result<ParsedCommand<'b>, ParseError> {
        let param_parts = parse_input_parts(input);
        let mut param_parts = param_parts.into_iter();
        let command_name = param_parts.next().ok_or_else(|| ParseError::InvalidSyntax(0))?;
        let command = command_line.lookup_mut(command_name.as_str()).ok_or(ParseError::UnknownCommand)?;
        let mut peekable_parts = param_parts.peekable();
        let (mut selected_subcommand_id, mut param_types, mut options_data) =
            ("", &command.param_types, &command.options);

        if !command.subcommands.is_empty() {
            let (sub_id, sub) = command.subcommands.iter().try_fold(Option::None, |_, cmd| {
                let mut tmp_peekable_parts = peekable_parts.clone();
                let possible_alias = tmp_peekable_parts.next().ok_or(Option::None)?;

                for alias in cmd.aliases.iter() {
                    if alias == &possible_alias {
                        if cmd.subcommands.is_empty() {
                            peekable_parts = tmp_peekable_parts;

                            return Err(Option::Some((cmd.id.as_str(), cmd)));
                        } else {
                            let (found_match, res) = check_subcommands(&mut tmp_peekable_parts, cmd);

                            if found_match {
                                peekable_parts = tmp_peekable_parts;
                            }

                            return res;
                        }
                    }
                }

                return Ok(Option::None);
            })
            .err()
            .ok_or(ParseError::MissingSubcommand)?
            .ok_or(ParseError::MissingSubcommand)?;

            selected_subcommand_id = sub_id;
            param_types = &sub.param_types;
            options_data = &sub.options;
        }

        let mut parameters: Vec<Parameter> = Vec::new();
        let mut options: Vec<Opt> = Vec::new();
        let mut pending_opt: Option<OptionData> = Option::None;
        let mut param_idx = 0;

        'parts_loop: while let Some(part) = peekable_parts.next() {
            if let Some(opt) = pending_opt {
                let result = parse_param(part.to_owned(), &opt.param_kind).ok_or_else(|| ParseError::InvalidParameter(part.to_owned()))?;

                options.push(Opt {
                    data: opt,
                    parameter: Option::Some(result),
                });

                pending_opt = Option::None;
                
                continue;
            }

            let mut flag_type = FlagType::None;

            for (c_i, c) in part.chars().enumerate() {
                if c == '-' {
                    if c_i == 0 {
                        flag_type = FlagType::Short;
                        
                        continue;
                    }

                    if flag_type == FlagType::Short && c_i == 1 {
                        flag_type = FlagType::Long;

                        break;
                    }
                }

                if flag_type == FlagType::Short {
                    let opt_data = options_data
                        .iter()
                        .find(|o| o.short_flags.contains(&c))
                        .cloned()
                        .ok_or_else(|| ParseError::UnnecessaryFlag(c.to_string()))?;

                    if opt_data.param_kind == ParameterKind::None {
                        options.push(Opt {
                            data: opt_data,
                            parameter: Option::None,
                        });
                    } else {
                        let raw_param = part.get(c_i + 1..).unwrap();
                        
                        if raw_param.is_empty() { 
                            pending_opt = Option::Some(opt_data);
                        } else {
                            options.push(Opt {
                                parameter: Option::Some(parse_param(raw_param.to_owned(), &opt_data.param_kind)
                                               .ok_or_else(|| ParseError::InvalidParameter(raw_param.to_owned()))?
                                ), 
                                data: opt_data,
                            });
                        }

                        continue 'parts_loop;
                    }
                }
            }

            if flag_type == FlagType::Long {
                let mut part = part.clone();
                part.remove(0);
                part.remove(0);
                let opt_data = options_data
                    .iter()
                    .find(|o| o.long_flags.contains(&part))
                    .cloned()
                    .ok_or_else(|| ParseError::UnnecessaryFlag(part))?;

                if opt_data.param_kind == ParameterKind::None {
                    options.push(Opt {
                        data: opt_data,
                        parameter: Option::None,
                    });
                } else {
                    pending_opt = Option::Some(opt_data);
                }

                continue;
            }

            let param_type = param_types.get(param_idx).ok_or_else(|| ParseError::UnnecessaryParameter(part.to_string()))?;
            let param = parse_param(part.clone(), &param_type).ok_or_else(|| ParseError::InvalidParameter(part.to_owned()))?;

            parameters.push(param);
            param_idx += 1;           
        }

        if let Some(opt) = pending_opt {
            return Err(ParseError::MissingParameter(opt.param_kind));
        }

        Ok(ParsedCommand {
            name: command.name,
            subcommand_id: selected_subcommand_id,
            parameters,
            options,
        })
    }

    fn check_subcommands<'a>(
        input_parts: &mut std::iter::Peekable<std::vec::IntoIter<String>>,
        cmd: &'a Command<'a>,
    ) -> (
        bool,
        Result<Option<(&'a str, &'a Command<'a>)>, Option<(&'a str, &'a Command<'a>)>>,
    ) {
        if let Some(possible_alias) = input_parts.next() {
            for sub_sub in cmd.subcommands.iter() {
                for alias in sub_sub.aliases.iter() {
                    if alias == &possible_alias {
                        return (true, Err(Option::Some((&sub_sub.id, &sub_sub))));
                    }
                }

                let sub_sub_subcmds_result = check_subcommands(input_parts, &sub_sub);

                if let Err(res) = sub_sub_subcmds_result.1 {
                    if res.is_some() {
                        return sub_sub_subcmds_result;
                    }
                }
            }
        } else {
            return (false, Err(Option::None));
        }

        return (false, Ok(Option::None));
    }

    #[derive(PartialEq)]
    enum FlagType {
        None,
        Short,
        Long,
    }
    
    #[derive(PartialEq)]
    enum OptionParseStatus {
        Ready,
        Waiting,
        NeedsParameter,
    }

    pub fn parse_params_and_options(
        format: &str,
    ) -> Result<(Vec<ParameterKind>, Vec<OptionData>), String> {
        let mut parameter_kinds = Vec::new();
        let mut options = Vec::new();
        let mut pending_kind = ParameterKind::None;
        let mut parse_opts = false;
        let mut waiting_opt_desc = false;
        let mut opt_status = OptionParseStatus::Ready;
        let mut opt_flag_builder = String::new();
        let mut opt_short_flags = Vec::new();
        let mut opt_long_flags = Vec::new();

        for c in format.chars().filter(|c| !c.is_whitespace()) {
            let mut ignore_curr = false;

            if pending_kind != ParameterKind::None {
                if is_list_param(c) {
                    pending_kind = ParameterKind::List(Box::new(pending_kind));
                    ignore_curr = true;
                }

                if opt_status == OptionParseStatus::NeedsParameter {
                    options.push(OptionData {
                        long_flags: opt_long_flags.clone(),
                        short_flags: opt_short_flags.clone(),
                        param_kind: pending_kind.clone(),
                    });

                    opt_long_flags.clear();
                    opt_short_flags.clear();
                    opt_status = OptionParseStatus::Ready;
                } else {
                    parameter_kinds.push(pending_kind);
                }

                pending_kind = ParameterKind::None;
            }

            if c == '-' {
                parse_opts = true;
                
                continue;
            }

            if parse_opts {
                if !waiting_opt_desc {
                    if c == '(' {
                        if opt_status == OptionParseStatus::Waiting {
                            options.push(OptionData {
                                long_flags: opt_long_flags.clone(),
                                short_flags: opt_short_flags.clone(),
                                param_kind: pending_kind.clone(),
                            });

                            opt_long_flags.clear();
                            opt_short_flags.clear();
                        }

                        waiting_opt_desc = true;
                        
                        continue;
                    } else {
                        if opt_status == OptionParseStatus::Waiting {
                            let kind = get_param_kind(c);

                            if kind == ParameterKind::None {
                                return Err(
                                    "unexpected character after option".to_owned(),
                                );
                            } else {
                                pending_kind = kind;
                                opt_status = OptionParseStatus::NeedsParameter;
                            }

                            continue;
                        }

                        return Err(format!("unexpected character: {}", c));
                    }
                } else {
                    if c == ',' {
                        let opt_flag_len = opt_flag_builder.len();
                        
                        if opt_flag_len > 1 {
                            opt_long_flags.push(opt_flag_builder.to_owned());
                        } else if opt_flag_len == 1 {
                            opt_short_flags.push(opt_flag_builder.chars().next().unwrap());
                        }

                        opt_flag_builder.clear();
                    } else if c == ')' {
                        let opt_flag_len = opt_flag_builder.len();
                        
                        if opt_flag_len > 1 {
                            opt_long_flags.push(opt_flag_builder.to_owned());
                        } else if opt_flag_len == 1 {
                            opt_short_flags.push(opt_flag_builder.chars().next().unwrap());
                        }

                        opt_flag_builder.clear();
                        waiting_opt_desc = false;
                        opt_status = OptionParseStatus::Waiting;
                    } else {
                        opt_flag_builder.push(c);
                    }
                }
            } else if !ignore_curr {
                let kind = get_param_kind(c);

                if kind == ParameterKind::None {
                    return Err(format!("invalid parameter kind: {}", c));
                } else {
                    pending_kind = kind;
                }
            }
        }

        if !opt_flag_builder.is_empty() {
            let opt_flag_len = opt_flag_builder.len();
            
            if opt_flag_len > 1 {
                opt_long_flags.push(opt_flag_builder.to_owned());
            } else if opt_flag_len == 1 {
                opt_short_flags.push(opt_flag_builder.chars().next().unwrap());
            }
        }

        if opt_status != OptionParseStatus::Ready {
            options.push(OptionData {
                long_flags: opt_long_flags,
                short_flags: opt_short_flags,
                param_kind: pending_kind,
            });
        } else if pending_kind != ParameterKind::None {
            parameter_kinds.push(pending_kind);
        }

        return Ok((parameter_kinds, options));
    }

    fn is_list_param(c: char) -> bool {
        c == '[' || c == '{'
    }

    fn get_param_kind(c: char) -> ParameterKind {
        match c {
            's' => ParameterKind::Text,
            'i' => ParameterKind::I32,
            'u' => ParameterKind::U32,
            _ => ParameterKind::None,
        }
    }

    fn parse_input_parts(input: &str) -> Vec<String> {
        let mut param_parts = Vec::new();
        let mut curr_part = String::new();
        let mut is_in_compound_param = false;
        let mut was_in_compound_param = false;
        let mut last_char_is_backslash = false;

        for c in input.chars() {
            if last_char_is_backslash {
                if c == '\\' {
                    curr_part.push('\\');
                } else {
                    curr_part.push(c);
                }

                last_char_is_backslash = false;
            } else {
                match c {
                    '[' => {
                        curr_part.push(c);
                        is_in_compound_param = true;
                    }
                    ']' => {
                        curr_part.push(c);

                        if is_in_compound_param {
                            param_parts.push(curr_part.to_owned());
                            curr_part.clear();
                            is_in_compound_param = false;
                        }
                    }
                    '"' => {
                        is_in_compound_param = !is_in_compound_param;

                        if !is_in_compound_param {
                            param_parts.push(curr_part.to_owned());
                            curr_part.clear();
                            was_in_compound_param = true;
                        }
                    }
                    ' ' => {
                        if is_in_compound_param {
                            curr_part.push(c);
                        } else if !was_in_compound_param {
                            param_parts.push(curr_part.to_owned());
                            curr_part.clear();
                        } else {
                            was_in_compound_param = false;
                        }
                    }
                    '\\' => {
                        last_char_is_backslash = true;
                    }
                    _ => {
                        curr_part.push(c);
                    }
                }
            }
        }

        if !curr_part.is_empty() {
            param_parts.push(curr_part.to_owned());
        }

        param_parts
    }

    fn parse_param(raw_param: String, param_type: &ParameterKind) -> Option<Parameter> {
        match param_type {
            ParameterKind::Text => {
                return Some(Parameter { val: ParameterVal::Text(raw_param) });
            }
            ParameterKind::I32 => {
                return raw_param.parse::<i32>()
                    .ok()
                    .map(|v| Parameter { val: ParameterVal::I32(v) });
            }
            ParameterKind::U32 => {
                return raw_param.parse::<u32>()
                    .ok()
                    .map(|v| Parameter { val: ParameterVal::U32(v) });
            }
            ParameterKind::List(t) => {
                const ELEMENT_DELIMITER: char = ',';

                let mut chars = raw_param.chars();
                let first_char = chars.next().unwrap();

                if first_char != '{' && first_char != '[' {
                    return None;
                }

                let last_char = chars.next_back().unwrap();

                if last_char != '}' && last_char != ']' {
                    return None;
                }

                return raw_param
                    .split(ELEMENT_DELIMITER)
                    .map(str::trim)
                    .map(|e| parse_param(e.to_string(), t))
                    .collect::<Option<Vec<_>>>()
                    .map(|elements| Parameter { val: ParameterVal::List(elements) });
            }
            _ => {}
        };

        None
    }
}
