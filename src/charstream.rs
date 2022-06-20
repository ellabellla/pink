
pub struct CharStream {
    input: Vec<char>,
    index: usize
}

impl CharStream {
    pub fn new(input: &str) -> CharStream {
        CharStream { input: input.chars().collect(), index: 0 }
    }

    pub fn peek(&mut self) -> Option<char> {
        if self.index >= self.input.len() {
            None
        } else {
            Some((*self.input)[self.index])
        }
    }

    pub fn prev(&mut self) -> Option<char> {
        if self.index <= 0 {
            None
        } else {
            self.index -= 1;
            Some((*self.input)[self.index])
        }
    }

    pub fn seek(&mut self, index: usize) -> bool{
        if index >= self.input.len() {
            false
        } else {
            self.index = index;
            true
        }
    }

    pub fn pos(&mut self) -> usize {
        self.index
    }
}

impl Iterator for CharStream {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= (*self.input).len() {
            None
        } else {
            let curr = (*self.input)[self.index];
            self.index += 1;
            Some(curr)
        }
    }
}

#[cfg(test)]
mod char_stream_tests {
    use super::CharStream;

    const test_input: &str = stringify!(a test string);

    #[test]
    fn test_next_prev() {
        let mut stream = CharStream::new(test_input);

        for char in test_input.chars() {
            assert_eq!(char, stream.next().unwrap());
        }

        assert_eq!(None, stream.next());
        
        for char in test_input.chars().rev() {
            assert_eq!(char, stream.prev().unwrap());
        }

        assert_eq!(None, stream.prev());
    }

    #[test]
    fn test_peek() {
        let mut stream = CharStream::new(test_input);

        for _ in 0..test_input.len() {
            assert_eq!(stream.peek().unwrap(), stream.next().unwrap());
        }
    }

    #[test]
    fn test_seek_pos() {
        let mut stream = CharStream::new(test_input);

        for (i, char) in test_input.chars().enumerate() {
            stream.seek(i);
            assert_eq!(stream.next().unwrap(), char);
        }
    }
}