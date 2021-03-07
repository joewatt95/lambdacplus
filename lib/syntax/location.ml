type 'a located = { data: 'a; source_loc : Lexing.position * Lexing.position}

let locate data source_loc = {data; source_loc}
