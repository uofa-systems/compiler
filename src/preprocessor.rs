use std::collections::HashMap;
use std::fs;
use std::path::Path;

pub fn preprocess(src: &str, base_path: &Path) -> Result<String, String> {
    let mut lines = src.lines();
    let mut output = String::new();
    let mut defines: HashMap<String, String> = HashMap::new();

    while let Some(line) = lines.next() {
        let trimmed = line.trim();
        if trimmed.starts_with('#') {
            if trimmed.starts_with("#include") {
                let path_part = trimmed.trim_start_matches("#include").trim();
                let filename = if path_part.starts_with('"') && path_part.ends_with('"') {
                    &path_part[1..path_part.len() - 1]
                } else if path_part.starts_with('<') && path_part.ends_with('>') {
                    &path_part[1..path_part.len() - 1]
                } else {
                    return Err(format!("Invalid include: {}", trimmed));
                };

                let include_path = base_path.parent().unwrap_or(Path::new(".")).join(filename);
                let content = fs::read_to_string(&include_path).map_err(|e| {
                    format!("Could not read included file {:?}: {}", include_path, e)
                })?;

                let processed_include = preprocess(&content, &include_path)?;
                output.push_str(&processed_include);
                output.push('\n');
            } else if trimmed.starts_with("#define") {
                let parts: Vec<&str> = trimmed.split_whitespace().collect();
                if parts.len() >= 3 {
                    let key = parts[1].to_string();
                    let val = parts[2..].join(" ");
                    defines.insert(key, val);
                }
            }
        } else {
            let mut processed_line = line.to_string();
            for (k, v) in &defines {
                let pattern = format!(" {} ", k);
                let replacement = format!(" {} ", v);
                processed_line = processed_line.replace(&pattern, &replacement);

                if processed_line.starts_with(k) {
                    processed_line = processed_line.replacen(k, v, 1);
                }
                if processed_line.ends_with(k) {
                    let start = processed_line.len() - k.len();
                    processed_line.replace_range(start.., v);
                }
            }
            output.push_str(&processed_line);
            output.push('\n');
        }
    }
    Ok(output)
}
