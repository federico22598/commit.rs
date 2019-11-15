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

    pub fn run(&mut self, input: &str) -> ExecResult {
        let parsed_syntax_res = parsing::parse(self, input);

        if parsed_syntax_res.is_err() {
            return ExecResult::Err(parsed_syntax_res.err().unwrap());
        }

        let parsed_syntax = parsed_syntax_res.ok().unwrap();

        ExecResult::Ok {
            subcommand: parsed_syntax.subcommand_id,
            command: parsed_syntax.command_name,
            parameters: parsed_syntax.parameters,
            options: parsed_syntax.options,
        }
    }
}

pub enum ExecResult<'a> {
    Err(ParseError),
    Ok {
        command: &'a str,
        subcommand: &'a str,
        parameters: Vec<Parameter>,
        options: Vec<Opt>,
    },
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
            ParameterVal::Text(s) => write!(f, "{}", s.as_str()),
            ParameterVal::I32(n) => write!(f, "{}", n.to_string()),
            ParameterVal::U32(n) => write!(f, "{}", n.to_string()),
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
                f.write_str(res.as_str())
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

                    let element_type = ParameterKind::from_str(raw_element_type.as_str());

                    if element_type.is_ok() {
                        return Ok(ParameterKind::List(Box::new(element_type.unwrap())));
                    }
                }

                return Err(());
            }
        }
    }
}

pub struct OptionData {
    pub short_flags: Vec<char>,
    pub long_flags: Vec<String>,
    pub param_kind: ParameterKind,
}

pub struct Opt {
    data: std::rc::Rc<OptionData>,
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
    options: Vec<std::rc::Rc<OptionData>>,
}

impl<'a> Command<'a> {
    pub fn new(aliases: Vec<&'a str>) -> Self {
        let tmp_aliases = aliases.clone();
        let name = tmp_aliases
            .get(0)
            .expect("at least one alias must be specified");

        if name.is_empty() {
            panic!("at least one alias must be specified");
        }

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
        let parse_result = parsing::parse_params_and_options(format);

        if let Err(e) = parse_result {
            panic!(e);
        }

        let (mut param_types, mut options) = parse_result.unwrap();

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

mod parsing {
    use super::*;

    pub struct SyntaxTree<'a> {
        pub command_name: &'a str,
        pub subcommand_id: &'a str, // Option<&'a str>,
        pub parameters: Vec<Parameter>,
        pub options: Vec<Opt>,
    }

    pub fn parse<'b, 'a: 'b>(
        command_line: &'b mut CommandLine<'a>,
        input: &str,
    ) -> Result<SyntaxTree<'b>, ParseError> {
        let param_parts = parse_input_parts(input);
        let mut param_parts = param_parts.into_iter();
        let command_name_res = param_parts.next();

        if command_name_res.is_none() {
            return Err(ParseError::InvalidSyntax(0));
        }

        let command_name = command_name_res.unwrap();
        let command_res = command_line.lookup_mut(command_name.as_str());

        if command_res.is_none() {
            return Err(ParseError::UnknownCommand);
        }

        let command = command_res.unwrap();
        let mut peekable_parts = param_parts.peekable();
        let (mut selected_subcommand_id, mut param_types, mut options_data) =
            ("", &command.param_types, &command.options);

        if !command.subcommands.is_empty() {
            let selected_subcommand_lookup_res =
                command.subcommands.iter().try_fold(Option::None, |_, cmd| {
                    let mut tmp_peekable_parts = peekable_parts.clone();
                    let possible_alias_res = tmp_peekable_parts.next();

                    if possible_alias_res.is_none() {
                        return Err(Option::None);
                    }

                    let possible_alias = possible_alias_res.unwrap();

                    for alias in cmd.aliases.iter() {
                        if alias == &possible_alias {
                            if cmd.subcommands.is_empty() {
                                peekable_parts = tmp_peekable_parts;

                                return Err(Option::Some((cmd.id.as_str(), cmd)));
                            } else {
                                let (found_match, res) =
                                    check_subcommands(&mut tmp_peekable_parts, cmd);

                                if found_match {
                                    peekable_parts = tmp_peekable_parts;
                                }

                                return res;
                            }
                        }
                    }

                    return Ok(Option::None);
                });

            let selected_subcommand_pair_res = if selected_subcommand_lookup_res.is_ok() {
                selected_subcommand_lookup_res.ok().unwrap()
            } else {
                selected_subcommand_lookup_res.err().unwrap()
            };

            if let Some((id, cmd)) = selected_subcommand_pair_res {
                selected_subcommand_id = id;
                param_types = &cmd.param_types;
                options_data = &cmd.options;
            } else {
                return Result::Err(ParseError::MissingSubcommand);
            }
        }

        let mut parameters: Vec<Parameter> = Vec::new();
        let mut options: Vec<Opt> = Vec::new();
        let mut pending_opt: Option<std::rc::Rc<OptionData>> = Option::None;
        let mut param_idx = 0;

        'parts_loop: while let Some(part) = peekable_parts.next() {
            if pending_opt.is_some() {
                let opt = pending_opt.unwrap();
                let result = parse_param(part.to_owned(), &opt.param_kind);

                if result.is_none() {
                    return Err(ParseError::InvalidParameter(part.to_owned()));
                }

                options.push(Opt {
                    data: opt,
                    parameter: Option::Some(result.unwrap()),
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
                    let found_option = options_data
                        .iter()
                        .find(|o| o.short_flags.contains(&c))
                        .cloned();

                    if found_option.is_none() {
                        return Err(ParseError::UnnecessaryFlag(c.to_string())); //TODO: ignore?
                    }

                    let found_option = found_option.unwrap();

                    if found_option.param_kind == ParameterKind::None {
                        options.push(Opt {
                            data: found_option,
                            parameter: Option::None,
                        });
                    } else {
                        let raw_param = part.get(c_i + 1..).unwrap();
                        
                        if raw_param.is_empty() { 
                            pending_opt = Option::Some(found_option);
                        } else {
                            let result = parse_param(raw_param.to_owned(), &found_option.param_kind);

                            if result.is_none() {
                                return Err(ParseError::InvalidParameter(raw_param.to_owned()));
                            }
                           
                            options.push(Opt {
                                data: found_option,
                                parameter: Option::Some(result.unwrap()),
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
                let found_option = options_data
                    .iter()
                    .find(|o| o.long_flags.contains(&part))
                    .cloned();

                if found_option.is_none() {
                    return Err(ParseError::UnnecessaryFlag(part));
                }

                let found_option = found_option.unwrap();

                if found_option.param_kind == ParameterKind::None {
                    options.push(Opt {
                        data: found_option,
                        parameter: Option::None,
                    });
                } else {
                    pending_opt = Option::Some(found_option);
                }

                continue;
            }

            let param_type_res = param_types.get(param_idx);

            if param_type_res.is_none() {
                return Err(ParseError::UnnecessaryParameter(part.to_string()));
            }

            let param_type = param_type_res.unwrap();
            let result = parse_param(part.clone(), &param_type);

            if result.is_none() {
                return Err(ParseError::InvalidParameter(part.to_owned()));
            }

            parameters.push(result.unwrap());
            param_idx += 1;           
        }

        if pending_opt.is_some() {
            let opt = pending_opt.unwrap();
            return Err(ParseError::MissingParameter(opt.param_kind.clone()));
        }

        return Ok(SyntaxTree {
            command_name: command.name,
            subcommand_id: selected_subcommand_id,
            parameters,
            options,
        });
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
                        return (true, Result::Err(Option::Some((&sub_sub.id, &sub_sub))));
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
            return (false, Result::Err(Option::None));
        }

        return (false, Result::Ok(Option::None));
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
    ) -> Result<(Vec<ParameterKind>, Vec<std::rc::Rc<OptionData>>), String> {
        let mut parameter_kinds = Vec::new();
        let mut options = Vec::new();
        let mut pending_kind = ParameterKind::None;
        let mut parse_opts = false;
        let mut waiting_opt_desc = false;
        let mut opt_status = OptionParseStatus::Ready;
        let mut opt_flag_builder = String::new();
        let mut opt_short_flags = Vec::new();
        let mut opt_long_flags = Vec::new();

        for c in format.chars() {
            if c.is_whitespace() {
                continue;
            }

            let mut ignore_curr = false;

            if pending_kind != ParameterKind::None {
                if is_list_param(c) {
                    pending_kind = ParameterKind::List(Box::new(pending_kind));
                    ignore_curr = true;
                }

                if opt_status == OptionParseStatus::NeedsParameter {
                    options.push(std::rc::Rc::new(OptionData {
                        long_flags: opt_long_flags.clone(),
                        short_flags: opt_short_flags.clone(),
                        param_kind: pending_kind.clone(),
                    }));

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
                            options.push(std::rc::Rc::new(OptionData {
                                long_flags: opt_long_flags.clone(),
                                short_flags: opt_short_flags.clone(),
                                param_kind: pending_kind.clone(),
                            }));

                            opt_long_flags.clear();
                            opt_short_flags.clear();
                        }

                        waiting_opt_desc = true;
                        
                        continue;
                    } else {
                        if opt_status == OptionParseStatus::Waiting {
                            let kind = get_param_kind(c);

                            if kind == ParameterKind::None {
                                return Result::Err(
                                    "unexpected character after option".to_owned(),
                                );
                            } else {
                                pending_kind = kind;
                                opt_status = OptionParseStatus::NeedsParameter;
                            }

                            continue;
                        }

                        return Result::Err(format!("unexpected character: {}", c));
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
                    return Result::Err(format!("invalid parameter kind: {}", c));
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
            options.push(std::rc::Rc::new(OptionData {
                long_flags: opt_long_flags,
                short_flags: opt_short_flags,
                param_kind: pending_kind.clone(),
            }));
        } else if pending_kind != ParameterKind::None {
            parameter_kinds.push(pending_kind);
        }

        return Result::Ok((parameter_kinds, options));
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
        let mut last_char_was_backslash = false;

        for (i, c) in input.chars().enumerate() {
            if last_char_was_backslash {
                if c == '\\' {
                    curr_part.push('\\');
                } else {
                    curr_part.push(c);
                }

                last_char_was_backslash = false;
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
                        last_char_was_backslash = true;
                    }
                    _ => {
                        curr_part.push(c);
                    }
                }
            }

            if i == input.len() - 1 && !curr_part.is_empty() {
                param_parts.push(curr_part.to_owned());

                break;
            }
        }

        param_parts
    }

    fn parse_param(raw_param: String, param_type: &ParameterKind) -> Option<Parameter> {
        match param_type {
            ParameterKind::Text => {
                return Some(Parameter {
                    val: ParameterVal::Text(raw_param),
                });
            }
            ParameterKind::I32 => {
                let num_parse_res = raw_param.parse::<i32>();

                if num_parse_res.is_err() {
                    return None;
                }

                return Some(Parameter {
                    val: ParameterVal::I32(num_parse_res.unwrap()),
                });
            }
            ParameterKind::U32 => {
                let num_parse_res = raw_param.parse::<u32>();

                if num_parse_res.is_err() {
                    return None;
                }

                return Some(Parameter {
                    val: ParameterVal::U32(num_parse_res.unwrap()),
                });
            }
            ParameterKind::List(t) => {
                const ELEMENT_DELIMITER: char = ',';

                let mut chars = raw_param.chars();
                let open_char = chars.next().unwrap();

                if open_char != '{' && open_char != '[' {
                    return None;
                }

                let close_char = chars.next_back().unwrap();

                if close_char != '}' && close_char != ']' {
                    return None;
                }

                let raw_param = chars.as_str();
                let raw_elements: Vec<&str> = raw_param.split(ELEMENT_DELIMITER).collect();
                let mut elements: Vec<Parameter> = Vec::new();

                for e in raw_elements {
                    let e = e.trim();
                    let res = parse_param(e.to_string(), t);

                    if res.is_none() {
                        return None;
                    }

                    elements.push(res.unwrap());
                }

                return Some(Parameter {
                    val: ParameterVal::List(elements),
                });
            }
            _ => {}
        };

        return None;
    }
}
