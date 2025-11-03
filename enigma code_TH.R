# Virtual Enigma Machine with 5 Wheels in R

# Set up the alphabet
alphabet <- LETTERS

# Create 5 wheels - each wheel maps input letters to different output letters
# Each wheel is a permutation of the alphabet
set.seed(42)  # For reproducible results

# Generate 5 different wheel configurations
wheel1 <- sample(alphabet)
wheel2 <- sample(alphabet)
wheel3 <- sample(alphabet)
wheel4 <- sample(alphabet)
wheel5 <- sample(alphabet)

# Store wheels in a list for easier manipulation
wheels <- list(wheel1, wheel2, wheel3, wheel4, wheel5)

# Display the initial wheel configurations
cat("Initial Wheel Configurations:\n")
for(i in 1:5) {
  cat("Wheel", i, ":\n")
  cat("Input: ", paste(alphabet, collapse=" "), "\n")
  cat("Output:", paste(wheels[[i]], collapse=" "), "\n\n")
}

# Initialize wheel positions (how much each wheel has rotated)
wheel_positions <- c(0, 0, 0, 0, 0)

# Function to rotate a wheel by n positions
rotate_wheel <- function(wheel, positions) {
  n <- length(wheel)
  positions <- positions %% n  # Handle rotations > 26
  if(positions == 0) return(wheel)
  c(wheel[(positions + 1):n], wheel[1:positions])
}

# Function to encode a letter through all wheels
encode_letter <- function(letter, wheels, positions) {
  # Convert letter to position (A=1, B=2, etc.)
  pos <- which(alphabet == letter)
  
  # Pass through each wheel (considering current rotation)
  for(i in 1:5) {
    rotated_wheel <- rotate_wheel(wheels[[i]], positions[i])
    pos <- which(alphabet == rotated_wheel[pos])
  }
  
  return(alphabet[pos])
}

# Function to decode a letter (reverse the encoding process)
decode_letter <- function(letter, wheels, positions) {
  # Convert letter to position (A=1, B=2, etc.)
  pos <- which(alphabet == letter)
  
  # Pass through each wheel in reverse order (5 to 1)
  for(i in 5:1) {
    rotated_wheel <- rotate_wheel(wheels[[i]], positions[i])
    # Find which input position gives this output
    pos <- which(rotated_wheel == alphabet[pos])
  }
  
  return(alphabet[pos])
}

# Function to advance the wheels (like odometer)
advance_wheels <- function(positions) {
  # Start with the rightmost wheel (wheel 5)
  positions[5] <- positions[5] + 1
  
  # Check for carry-over (when a wheel completes full rotation)
  for(i in 5:2) {
    if(positions[i] >= 26) {
      positions[i] <- 0
      positions[i-1] <- positions[i-1] + 1
    }
  }
  
  # Handle leftmost wheel overflow
  if(positions[1] >= 26) {
    positions[1] <- 0
  }
  
  return(positions)
}

# Demonstration: Show wheel advancement for first 30 steps
cat("Wheel Position Advancement (first 30 steps):\n")
cat("Step | Wheel1 | Wheel2 | Wheel3 | Wheel4 | Wheel5\n")
cat("-----|--------|--------|--------|--------|--------\n")

demo_positions <- c(0, 0, 0, 0, 0)
for(step in 0:29) {
  cat(sprintf("%4d |   %2d   |   %2d   |   %2d   |   %2d   |   %2d\n", 
              step, demo_positions[1], demo_positions[2], demo_positions[3], 
              demo_positions[4], demo_positions[5]))
  demo_positions <- advance_wheels(demo_positions)
}

# Encrypt the specified message with custom starting positions
message <- "SUBLIME IS THE NIGHT ON THE FIRST OF OCTOBER"
cat("\nEncrypting message:", message, "\n")

# Set custom starting positions
wheel_positions <- c(5, 4, 2, 10, 3)
cat("Starting wheel positions:", paste(wheel_positions, collapse=","), "\n\n")

# Remove spaces and convert to uppercase
clean_message <- gsub(" ", "", toupper(message))
encoded_message <- ""

cat("Letter-by-letter encryption:\n")
for(i in 1:nchar(clean_message)) {
  char <- substr(clean_message, i, i)
  
  # Advance wheels before encoding each letter
  wheel_positions <- advance_wheels(wheel_positions)
  
  # Encode the letter
  encoded_char <- encode_letter(char, wheels, wheel_positions)
  encoded_message <- paste0(encoded_message, encoded_char)
  
  cat(sprintf("Step %2d: %s -> Position: [%2d,%2d,%2d,%2d,%2d] -> %s\n", 
              i, char, wheel_positions[1], wheel_positions[2], 
              wheel_positions[3], wheel_positions[4], wheel_positions[5], encoded_char))
}

cat("\nOriginal message:", message, "\n")
cat("Cleaned message: ", clean_message, "\n")
cat("Encoded message: ", encoded_message, "\n")

# Decrypt the given message
encrypted_message <- "CWZNCPSDPTIHTHWTXPCSLOUYFKXDBVWKPCSW"
cat("\nDecrypting message:", encrypted_message, "\n")

# Reset to starting positions for decryption
wheel_positions <- c(5, 4, 2, 10, 3)
cat("Starting wheel positions:", paste(wheel_positions, collapse=","), "\n\n")

decoded_message <- ""

cat("Letter-by-letter decryption:\n")
for(i in 1:nchar(encrypted_message)) {
  char <- substr(encrypted_message, i, i)
  
  # Advance wheels before decoding each letter (same as encoding)
  wheel_positions <- advance_wheels(wheel_positions)
  
  # Decode the letter
  decoded_char <- decode_letter(char, wheels, wheel_positions)
  decoded_message <- paste0(decoded_message, decoded_char)
  
  cat(sprintf("Step %2d: %s -> Position: [%2d,%2d,%2d,%2d,%2d] -> %s\n", 
              i, char, wheel_positions[1], wheel_positions[2], 
              wheel_positions[3], wheel_positions[4], wheel_positions[5], decoded_char))
}

cat("\nEncrypted message:", encrypted_message, "\n")
cat("Decrypted message:", decoded_message, "\n")

# Check if the decrypted message contains "SUBLIME"
if(grepl("SUBLIME", decoded_message)) {
  cat("\n✓ SUCCESS: The decrypted message DOES contain the word 'SUBLIME'\n")
  # Find the position of SUBLIME
  sublime_pos <- regexpr("SUBLIME", decoded_message)
  cat("'SUBLIME' found at position:", sublime_pos[1], "\n")
} else {
  cat("\n✗ The decrypted message does NOT contain the word 'SUBLIME'\n")
}

# Brute force decrypt function to find rotor settings
brute_force_decrypt <- function(encrypted_message, target_word = "SUBLIME") {
  cat("Starting brute force decryption...\n")
  cat("Testing all possible 5-wheel starting positions...\n")
  cat("Total combinations to test:", 26^5, "\n\n")
  
  combinations_tested <- 0
  start_time <- Sys.time()
  
  # Test all possible starting positions for 5 wheels (26^5 = 11,881,376 combinations)
  for(w1 in 0:25) {
    for(w2 in 0:25) {
      for(w3 in 0:25) {
        for(w4 in 0:25) {
          for(w5 in 0:25) {
            combinations_tested <- combinations_tested + 1
            
            # Progress indicator (every 100,000 attempts)
            if(combinations_tested %% 100000 == 0) {
              elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
              rate <- combinations_tested / elapsed
              remaining <- (26^5 - combinations_tested) / rate
              cat(sprintf("Progress: %d/%d (%.2f%%) - Rate: %.0f/sec - Est. remaining: %.1f min\n", 
                          combinations_tested, 26^5, 
                          (combinations_tested/26^5)*100, rate, remaining/60))
            }
            
            # Test this combination
            test_positions <- c(w1, w2, w3, w4, w5)
            wheel_positions <- test_positions
            decoded_message <- ""
            
            # Decrypt the message with these positions
            for(i in 1:nchar(encrypted_message)) {
              char <- substr(encrypted_message, i, i)
              wheel_positions <- advance_wheels(wheel_positions)
              decoded_char <- decode_letter(char, wheels, wheel_positions)
              decoded_message <- paste0(decoded_message, decoded_char)
            }
            
            # Check if target word is found
            if(grepl(target_word, decoded_message)) {
              elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
              
              cat("\n" , rep("=", 60), "\n")
              cat("SUCCESS! Found matching rotor settings!\n")
              cat(rep("=", 60), "\n")
              cat("Starting positions: [", paste(test_positions, collapse=", "), "]\n")
              cat("Combinations tested:", combinations_tested, "\n")
              cat("Time elapsed:", sprintf("%.2f", elapsed), "seconds\n")
              cat("Encrypted message:", encrypted_message, "\n")
              cat("Decrypted message:", decoded_message, "\n")
              cat("Target word '", target_word, "' found at position:", 
                  regexpr(target_word, decoded_message)[1], "\n")
              
              # Try to add spaces to make it more readable
              cat("\nFormatted decrypted message (attempting word breaks):\n")
              # Simple heuristic formatting - in practice you'd use dictionary lookup
              formatted <- decoded_message
              # Look for common patterns and try to insert spaces
              formatted <- gsub("SUBLIME", "SUBLIME ", formatted)
              formatted <- gsub("IS", " IS ", formatted)
              formatted <- gsub("THE", " THE ", formatted)
              formatted <- gsub("ON", " ON ", formatted)
              formatted <- gsub("OF", " OF ", formatted)
              formatted <- gsub("FIRST", " FIRST ", formatted)
              formatted <- gsub("NIGHT", " NIGHT ", formatted)
              formatted <- gsub("OCTOBER", " OCTOBER ", formatted)
              formatted <- gsub("\\s+", " ", formatted)  # Clean up multiple spaces
              formatted <- trimws(formatted)  # Trim leading/trailing spaces
              
              cat("Formatted:", formatted, "\n")
              
              return(list(
                positions = test_positions,
                decrypted = decoded_message,
                formatted = formatted,
                combinations_tested = combinations_tested,
                time_elapsed = elapsed
              ))
            }
          }
        }
      }
    }
  }
  
  # If we get here, no solution was found
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("\nNo solution found after testing all", combinations_tested, "combinations\n")
  cat("Time elapsed:", sprintf("%.2f", elapsed), "seconds\n")
  return(NULL)
}

# Optimized version that tries more likely positions first
brute_force_decrypt_smart <- function(encrypted_message, target_word = "SUBLIME") {
  cat("Starting smart brute force decryption...\n")
  cat("Testing positions around common starting values first...\n\n")
  
  combinations_tested <- 0
  start_time <- Sys.time()
  
  # Common starting positions to try first (often 0 or small numbers)
  priority_values <- c(0:10, 25, 24, 23)  # Start with 0-10, then end positions
  
  # First, try combinations with priority values
  cat("Phase 1: Testing likely starting positions...\n")
  for(w1 in priority_values) {
    for(w2 in priority_values) {
      for(w3 in priority_values) {
        for(w4 in priority_values) {
          for(w5 in priority_values) {
            combinations_tested <- combinations_tested + 1
            
            if(combinations_tested %% 1000 == 0) {
              cat("Tested", combinations_tested, "combinations...\n")
            }
            
            test_positions <- c(w1, w2, w3, w4, w5)
            wheel_positions <- test_positions
            decoded_message <- ""
            
            for(i in 1:nchar(encrypted_message)) {
              char <- substr(encrypted_message, i, i)
              wheel_positions <- advance_wheels(wheel_positions)
              decoded_char <- decode_letter(char, wheels, wheel_positions)
              decoded_message <- paste0(decoded_message, decoded_char)
            }
            
            if(grepl(target_word, decoded_message)) {
              elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
              
              cat("\n" , rep("=", 60), "\n")
              cat("SUCCESS! Found matching rotor settings!\n")
              cat(rep("=", 60), "\n")
              cat("Starting positions: [", paste(test_positions, collapse=", "), "]\n")
              cat("Combinations tested:", combinations_tested, "\n")
              cat("Time elapsed:", sprintf("%.2f", elapsed), "seconds\n")
              cat("Decrypted message:", decoded_message, "\n")
              
              return(list(
                positions = test_positions,
                decrypted = decoded_message,
                combinations_tested = combinations_tested,
                time_elapsed = elapsed
              ))
            }
          }
        }
      }
    }
  }
  
  cat("Phase 1 complete. No solution found in priority positions.\n")
  cat("Phase 2: Testing all remaining combinations...\n")
  
  # If not found in priority positions, test all combinations
  return(brute_force_decrypt(encrypted_message, target_word))
}

# Run the smart brute force attack
cat("\nStarting brute force attack on message:", encrypted_message, "\n")
result <- brute_force_decrypt_smart(encrypted_message, "SUBLIME")

# Function to demonstrate the wheel mechanism more clearly
demonstrate_wheel_rotation <- function(num_rotations = 60) {
  cat("\nDemonstrating wheel rotation mechanism:\n")
  cat("(Wheel 5 advances each step, others advance when previous wheel completes rotation)\n\n")
  
  positions <- c(0, 0, 0, 0, 0)
  
  for(i in 1:num_rotations) {
    positions <- advance_wheels(positions)
    
    # Show key transition points
    if(positions[5] == 0 || positions[4] == 0 || positions[3] == 0 || 
       positions[2] == 0 || i <= 5 || i %% 26 == 0) {
      cat(sprintf("Step %2d: [%2d, %2d, %2d, %2d, %2d]", 
                  i, positions[1], positions[2], positions[3], 
                  positions[4], positions[5]))
      if(positions[5] == 0) cat(" <- Wheel 5 completed rotation")
      if(positions[4] == 0 && positions[5] == 0) cat(" <- Wheel 4 advanced")
      if(positions[3] == 0 && positions[4] == 0 && positions[5] == 0) cat(" <- Wheel 3 advanced")
      cat("\n")
    }
  }
}

# Run the demonstration
demonstrate_wheel_rotation()