//! The module for the constant pool, which is used to reference literals.

/// The struct for the constant pool.
pub struct ConstantPool
{
    pub pool: Vec<u8>,
    pub ptr_size: u8,
}

impl ConstantPool
{
    /// Access an int from the pool. Index is the address of the initial byte.
    pub fn get_int(&self, index: usize) -> Option<i32>
    {
        if index + 4 > self.pool.len() { return None; }
        let bytes: &[u8] = &self.pool[index..index+4];
        Some(i32::from_le_bytes(bytes.try_into().expect("4 bytes returned")))
    }

    /// Add an int to the pool if it is not already present. Return it's address, or an error if the pool is full.
    pub fn insert_int(&mut self, value: i32) -> Result<usize, ()>
    {
        for index in 0..self.pool.len()/4
        {
            let index: usize = index * 4;
            if self.get_int(index) == Some(value)
            {
                return Ok(index);
            }
        }

        if self.pool.len() > (1 << self.ptr_size) - 4
        {
            return Err(());
        }

        while self.pool.len() % 4 != 0 { self.pool.push(0); }
        self.pool.append(&mut value.to_le_bytes().to_vec());
        Ok(self.pool.len()-4)
    }
}