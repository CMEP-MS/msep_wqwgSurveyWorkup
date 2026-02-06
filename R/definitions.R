# column renaming ----
column_name_lookup <- tribble(
    ~"Raw", ~"Cleaned",
    "Your name", "Name",
    "Email address", "Email",
    "Are you interested in participating in the Water Quality Working Group, and if so, at what level?", "Participation",
    "How do you classify your involvement with water quality work? Check all that apply.", "Involvement",
    "Which techniques/data types do you collect/generate or use in your decision-making? Check all that apply.", "Techniques",
    "Which water quality topics do you work with? Check all that apply.", "Topics"
)
name_map <- setNames(column_name_lookup$Cleaned, column_name_lookup$Raw)


# multiple choice options from the survey ----
define_participation <- function(value){
    # input needs to be a vector of values
    dplyr::case_when(str_starts(value, "No, and don't") ~ "none",
                     str_starts(value, "No, but put me") ~ "email only",
                     str_starts(value, "Yes") ~ "full",
                     .default = "PROBLEM")
}

categories_involvement <- c("Management", "Research and Monitoring", "Engineering",
                            "WQ is not my focus but I collect data")

categories_techniques <- c("Deployed instrumentation", "Handheld instrumentation",
                           "Grab samples", "Remote Sensing", "Modeling/Forecasting")

categories_topics <- c("Salinity", "DO/Hypoxia", "pH/Ocean acificiation",
                       "pH/Ocean acidification",  # fixing the typo in some places
                       "Water Clarity", "Nutrients/chlorophyll/eutrophication",
                       "Harmful algal blooms (HABs)",
                       "Phytoplankton (may or may not include HABs)",
                       "Fecal Indicator Bacteria",
                       "Other Bacteria",
                       "Microplastics", "Heavy Metals", "Pesticides",
                       "Stormwater", "Sedimentation",
                       "Contaminants of Emerging Concern", "Legacy Contaminants")


# summarize categories function
summarize_categories <- function(data, column, known_categories) {
    data |> 
        mutate({{ column }} := case_when(
            !({{ column }} %in% known_categories) ~ "Other",
            .default = {{ column }}
        )) |> 
        count({{ column }}, sort = TRUE) |> 
        mutate({{ column }} := fct_inorder({{ column }}),
               {{ column }} := fct_relevel({{ column }}, "Other", after = Inf)) |> 
        arrange({{ column }})
}


# organization wrangling ----
simplify_organization <- function(value){
    # input needs to be a vector of values
    dplyr::case_when(str_detect(value, "MDEQ|Department of Environmental Quality") ~ "Mississippi Department of Environmental Quality",
                     str_detect(value, "Department of Marine Resources|MDMR") ~ "Mississippi Department of Marine Resources",
                     str_detect(value, "Mississippi State University|Mississippi Sate University|MSU") ~ "Mississippi State University",
                     str_detect(value, "USM|University of Southern Mississippi") ~ "University of Southern Mississippi",
                     .default = value)
}


