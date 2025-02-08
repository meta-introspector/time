from datetime import datetime

current_time = datetime.utcnow()

# Define the epoch time and the target time
target_time = datetime(2025, 1, 15, 20, 53, 1)

# Calculate the difference in seconds using timestamp()
#seconds_since_epoch = target_time.timestamp()

# Calculate the difference in seconds manually
delta = current_time - target_time
seconds_since_epoch_manual = delta.total_seconds()
print("Seconds :", seconds_since_epoch_manual)
print("Minutes :", seconds_since_epoch_manual/60)
print("Hours :", seconds_since_epoch_manual/60/60)

# Calculate days, weeks, and months
days_to_target = delta.days
weeks_to_target = days_to_target // 7
months_to_target = days_to_target // 30  # Approximation

print(f"Days : {days_to_target}")
print(f"Weeks : {weeks_to_target}")
print(f"Months Aprox: {months_to_target}")
