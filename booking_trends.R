# =============================
#   OPTIONS (For Windows Device Display)
# =============================
options(device = "windows")

# =============================
#   Load Libraries
# =============================
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation (mutate, filter, summarise, etc.)
library(stringr)   # For string trimming and text cleaning
library(lubridate) # For date/time parsing
library(tidyr)     # For separating multi-value cells

# =============================
#   Read Excel File
# =============================
# Load the raw dataset from Excel
df <- read_excel("booking_trends_dirty.xlsx", sheet = "Sheet1")

# Preview raw data structure
head(df)
str(df)

# =============================
#   Data Cleaning & Transformation
# =============================
df <- df %>%
  mutate(
    # Convert booking and event dates into proper date-time formats
    BookingDate = parse_date_time(`Booking Date`, orders = c("ymd HMS", "ymd HM")),
    EventDate   = ymd(`Event Date`),
    
    # Clean and standardize Booked Services column
    BookedServices = str_trim(`Booked Services`),
    
    # Add helper columns for analysis
    DayOfWeek = weekdays(EventDate),          # Extract day name (e.g., Monday)
    Month     = format(EventDate, "%B"),      # Extract month name (e.g., January)
    
    # Categorize bookings into time slots (4 groups)
    TimeSlot = cut(
      as.numeric(format(BookingDate, "%H")),
      breaks = c(0, 6, 12, 18, 24),  
      labels = c("Early Morning", "Morning", "Afternoon", "Evening"),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# Preview cleaned data
head(df)
str(df)

# =============================
#   Summary Tables (for Trends)
# =============================

# 1. Most Requested Days
most_requested_days <- df %>%
  filter(!is.na(DayOfWeek)) %>%
  count(`Most Requested Days` = DayOfWeek, name = "Count", sort = TRUE)

# 2. Top 5 Most Booked Services
df_services <- df %>%
  separate_rows(BookedServices, sep = ",\\s*")

top_services <- df_services %>%
  filter(!is.na(BookedServices)) %>%
  count(`Top 5 Most Booked Services` = BookedServices, name = "Count", sort = TRUE) %>%
  slice_head(n = 5)

# 3. Most Booked Planner Packages
most_booked_packages <- df %>%
  filter(!is.na(`Booked Package`)) %>%
  count(`Most Booked Planner Package` = `Booked Package`, name = "Count", sort = TRUE)

# 4. Preferred Time Slots
preferred_time_slots <- df %>%
  filter(!is.na(TimeSlot)) %>%
  count(`Preferred Time Slots` = TimeSlot, name = "Count", sort = TRUE)

# =============================
#   Preview Results in Console
# =============================
most_requested_days
top_services
most_booked_packages
preferred_time_slots
