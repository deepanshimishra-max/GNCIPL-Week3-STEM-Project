###############################################################################
# PROJECT 37 - STEM EDUCATION: STUDENT PERFORMANCE IN STEM SUBJECTS
# GNCIPL - Data Analytics Week-3 Project (SQL + R)
#
# Phases covered:
#   Phase 1: Project Planning & Scoping
#   Phase 2: Data Collection & Storage (SQL using SQLite inside R)
#   Phase 3: Data Analysis & Modeling (R)
#   Phase 4: Reporting & Visualization (Graphs only)
#   Phase 5: Deliverables & Folder Structure
#   Phase 6: Iteration & Enhancement Notes
###############################################################################


## ============================================================
## PHASE 1: PROJECT PLANNING & SCOPING
## ============================================================
# 1.1 Project Topic : Student Performance in STEM Subjects
# 1.2 Objective     : Identify which factors (attendance, study hours,
#                     extra classes, parental education) most influence
#                     students' performance in STEM subjects.
# 1.3 Target Users  : Teachers / School Administrators / Policy Makers
# 1.4 Key Questions :
#       - Which factors affect STEM performance the most?
#       - How much impact do attendance and study hours have on scores?
#       - Do students who take extra classes perform better?


## ============================================================
## SETUP: Install (if needed) and load required packages
## ============================================================
required_packages <- c("RSQLite", "DBI", "dplyr", "tidyr", "ggplot2", "gridExtra")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

set.seed(42)


## ============================================================
## PHASE 2: DATA COLLECTION & STORAGE (SQL)
## ============================================================

# ---- 2.1 Data Source ----
# Realistic simulated dataset of 200 students.
# To use real data instead, comment out the simulation block and use:
# students_raw <- read.csv("your_school_data.csv")

n <- 200
students_raw <- data.frame(
  student_id           = 1:n,
  attendance_pct       = round(rnorm(n, mean = 82, sd = 10)),
  study_hours_per_week = round(rnorm(n, mean = 8, sd = 3), 1),
  extra_classes        = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.4, 0.6)),
  parental_education   = sample(c("School", "Graduate", "PostGraduate"),
                                n, replace = TRUE, prob = c(0.3, 0.45, 0.25))
)

# Keep values within realistic bounds
students_raw$attendance_pct       <- pmin(pmax(students_raw$attendance_pct, 40), 100)
students_raw$study_hours_per_week <- pmax(students_raw$study_hours_per_week, 0)

# Generate STEM scores as a function of the predictors plus random noise
extra_bonus  <- ifelse(students_raw$extra_classes == "Yes", 5, 0)
parent_bonus <- ifelse(students_raw$parental_education == "PostGraduate", 6,
                 ifelse(students_raw$parental_education == "Graduate", 3, 0))

students_raw$math_score <- round(
  40 + 0.35 * students_raw$attendance_pct +
  1.8 * students_raw$study_hours_per_week +
  extra_bonus + parent_bonus + rnorm(n, 0, 6))

students_raw$science_score <- round(
  38 + 0.32 * students_raw$attendance_pct +
  2.0 * students_raw$study_hours_per_week +
  extra_bonus + parent_bonus + rnorm(n, 0, 6))

students_raw$computer_score <- round(
  42 + 0.30 * students_raw$attendance_pct +
  1.6 * students_raw$study_hours_per_week +
  extra_bonus + parent_bonus + rnorm(n, 0, 6))

score_cols <- c("math_score", "science_score", "computer_score")
students_raw[score_cols] <- lapply(students_raw[score_cols], function(x) pmin(pmax(x, 0), 100))
students_raw$overall_stem_score <- round(rowMeans(students_raw[score_cols]), 1)

# ---- 2.2 Import to SQL (SQLite) ----
db_path <- "stem_project.db"
con <- dbConnect(RSQLite::SQLite(), db_path)

# ---- 2.3 Write table ----
dbWriteTable(con, "students", students_raw, overwrite = TRUE)

# ---- 2.4 Data Cleaning via SQL ----
dbExecute(con, "DELETE FROM students WHERE student_id IS NULL")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_student ON students(student_id)")

# ---- 2.5 SQL Queries ----
q1 <- dbGetQuery(con, "
  SELECT parental_education,
         ROUND(AVG(overall_stem_score), 2) AS avg_score,
         COUNT(*) AS total_students
  FROM students
  GROUP BY parental_education
  ORDER BY avg_score DESC
")
print(q1)

q2 <- dbGetQuery(con, "
  SELECT student_id, attendance_pct, study_hours_per_week,
         extra_classes, overall_stem_score
  FROM students
  ORDER BY overall_stem_score DESC
  LIMIT 10
")
print(q2)

q3 <- dbGetQuery(con, "
  SELECT student_id, attendance_pct, overall_stem_score
  FROM students
  WHERE attendance_pct < 70
  ORDER BY attendance_pct ASC
")
print(q3)

# Pull cleaned table back into R
data <- dbGetQuery(con, "SELECT * FROM students")
dbDisconnect(con)


## ============================================================
## PHASE 3: DATA ANALYSIS & MODELING (R)
## ============================================================

# ---- 3.1 Clean data in R ----
data <- data %>%
  mutate(
    extra_classes      = as.factor(extra_classes),
    parental_education = factor(parental_education,
                                levels = c("School", "Graduate", "PostGraduate"))
  ) %>%
  drop_na()

# ---- 3.2 Summary Statistics ----
print(summary(data[, c("attendance_pct", "study_hours_per_week", "overall_stem_score")]))

# ---- 3.3 Correlation Matrix ----
num_vars <- data[, c("attendance_pct", "study_hours_per_week",
                     "math_score", "science_score", "computer_score",
                     "overall_stem_score")]
print(round(cor(num_vars), 2))

# ---- 3.4 Linear Regression Model ----
model <- lm(overall_stem_score ~ attendance_pct + study_hours_per_week +
              extra_classes + parental_education, data = data)
print(summary(model))

# Predicted scores (for graph 5)
data$predicted_score <- predict(model, data)


## ============================================================
## PHASE 4: VISUALIZATION (Graphs only)
## ============================================================

plots_dir <- "plots"
if (!dir.exists(plots_dir)) dir.create(plots_dir)

# --- Graph 1: Attendance vs Overall Score ---
g1 <- ggplot(data, aes(x = attendance_pct, y = overall_stem_score)) +
  geom_point(color = "#2C7FB8", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#D7301F", se = FALSE) +
  labs(title = "Attendance vs Overall STEM Score",
       x = "Attendance (%)", y = "Overall STEM Score") +
  theme_minimal(base_size = 12)

# --- Graph 2: Study Hours vs Overall Score ---
g2 <- ggplot(data, aes(x = study_hours_per_week, y = overall_stem_score)) +
  geom_point(color = "#31A354", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#D7301F", se = FALSE) +
  labs(title = "Study Hours/Week vs Overall STEM Score",
       x = "Study Hours per Week", y = "Overall STEM Score") +
  theme_minimal(base_size = 12)

# --- Graph 3: Score distribution by Extra Classes ---
g3 <- ggplot(data, aes(x = extra_classes, y = overall_stem_score, fill = extra_classes)) +
  geom_boxplot() +
  labs(title = "Overall Score by Extra Classes",
       x = "Extra Classes Taken", y = "Overall STEM Score") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# --- Graph 4: Average score by Parental Education ---
g4 <- ggplot(q1, aes(x = parental_education, y = avg_score, fill = parental_education)) +
  geom_col() +
  geom_text(aes(label = avg_score), vjust = -0.4) +
  labs(title = "Average STEM Score by Parental Education",
       x = "Parental Education", y = "Average Score") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# --- Graph 5: Predicted vs Actual Score ---
g5 <- ggplot(data, aes(x = overall_stem_score, y = predicted_score)) +
  geom_point(color = "#756BB1", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual STEM Score",
       x = "Actual Score", y = "Predicted Score") +
  theme_minimal(base_size = 12)

# Save individual PNGs
ggsave(file.path(plots_dir, "01_attendance_vs_score.png"),  g1, width = 7, height = 5, dpi = 150)
ggsave(file.path(plots_dir, "02_studyhours_vs_score.png"),  g2, width = 7, height = 5, dpi = 150)
ggsave(file.path(plots_dir, "03_extraclasses_boxplot.png"), g3, width = 7, height = 5, dpi = 150)
ggsave(file.path(plots_dir, "04_parentaledu_bar.png"),      g4, width = 7, height = 5, dpi = 150)
ggsave(file.path(plots_dir, "05_predicted_vs_actual.png"),  g5, width = 7, height = 5, dpi = 150)

# Display graphs in RStudio Plots pane
print(g1)
print(g2)
print(g3)
print(g4)
print(g5)

# Combined dashboard — all 5 graphs in one image
combined_dashboard <- gridExtra::arrangeGrob(g1, g2, g3, g4, g5, ncol = 2)
grid::grid.newpage()
grid::grid.draw(combined_dashboard)

ggsave(file.path(plots_dir, "00_combined_dashboard.png"), combined_dashboard,
       width = 14, height = 12, dpi = 150)


## ============================================================
## PHASE 5: DELIVERABLES & FOLDER STRUCTURE
## ============================================================

folders <- c("SQL", "R_Scripts", "Data", "Documentation")
for (f in folders) if (!dir.exists(f)) dir.create(f)

file.copy("stem_project.db", file.path("SQL", "stem_project.db"), overwrite = TRUE)
write.csv(data, file.path("Data", "students_cleaned.csv"), row.names = FALSE)


## ============================================================
## PHASE 6: ITERATION & ENHANCEMENT
## ============================================================
# Future improvements:
# 1. Use a real school dataset (CSV/SQL) instead of simulated data.
# 2. Add more predictors: gender, internet access, sleep hours, etc.
# 3. Automate this R script using RStudio scheduled tasks or a cron job.
