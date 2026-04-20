# ═══════════════════════════════════════════════════════════════════════════════
# IMF WEO Executive Summary – Distributional Impact Analysis
#
# Who does the IMF say will be hurt most by the war, through what channel,
# and how explicitly does the text acknowledge the asymmetry?
# ═══════════════════════════════════════════════════════════════════════════════
# Packages required:
#   install.packages(c("pdftools", "quanteda", "stringr", "ellmer", "quallmer"))
# ───────────────────────────────────────────────────────────────────────────────

library(pdftools)   # PDF → character vector
library(quanteda)   # corpus, tokens, docvars
library(stringr)    # string cleaning helpers
library(ellmer)     # type_object(), type_boolean(), type_string(), type_enum()
library(quallmer)   # qlm_codebook(), qlm_code()
library(dplyr)      # mutate, across, left_join


# ── Working directory ──────────────────────────────────────────────────────────

wd <- "C:/Users/Patrick Shea/Dropbox/AI Upskilling/Walkthru/april26session"


## Task 1: Text ingestion, corpus construction, codebook definition


# ═══════════════════════════════════════════════════════════════════════════════
# 1.  READ AND CLEAN THE PDF
# ═══════════════════════════════════════════════════════════════════════════════

# pdf_text() returns one character string per page.
raw_pages <- pdf_text(file.path(wd, "execsum.pdf"))

# Collapse all pages into one string, separating pages with a blank line so
# the page boundary acts like a paragraph break during splitting below.
raw_text <- paste(raw_pages, collapse = "\n\n")
raw_text
# --- Minimal cleaning steps ---------------------------------------------------

# 1a. Remove isolated page numbers: lines that contain only digits (and spaces).
clean_text <- str_remove_all(raw_text, "(?m)^[ \t]*\\d+[ \t]*\n")

# 1b. Remove short all-caps lines that are typical IMF headers/footers
#     (e.g. "WORLD ECONOMIC OUTLOOK", "INTERNATIONAL MONETARY FUND").
#     The regex matches lines of 6+ chars that are entirely uppercase letters,
#     spaces, or hyphens.  Adjust if real headers look different after step 3.
clean_text <- str_remove_all(clean_text, "(?m)^[A-Z][A-Z \\-]{5,}[ \t]*\n")

# 1c. Collapse runs of 3 or more newlines down to exactly two.
#     Two newlines = paragraph break; within-paragraph line wraps stay as one \n.
clean_text <- str_replace_all(clean_text, "\n{3,}", "\n\n")

# 1d. Strip leading/trailing whitespace from the whole document.
clean_text <- str_trim(clean_text)

# Print so you can eyeball the result and spot any residual headers/footers.
cat("═══ CLEANED RAW TEXT ═══════════════════════════════════════════════════════\n")
cat(clean_text)
cat("\n════════════════════════════════════════════════════════════════════════════\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 2.  SPLIT INTO PARAGRAPHS AND BUILD A QUANTEDA CORPUS
# ═══════════════════════════════════════════════════════════════════════════════

# Split on one or more blank lines (the paragraph separator established above).
paragraphs <- str_split(clean_text, "\n{2,}")[[1]]

# Drop any entries that are empty or purely whitespace.
paragraphs <- paragraphs[str_trim(paragraphs) != ""]

# Within each paragraph, collapse soft line-breaks to a single space,
# then squish any duplicate spaces produced by the PDF extraction.
paragraphs <- str_replace_all(paragraphs, "\n", " ")
paragraphs <- str_squish(paragraphs)

# Build the corpus.  Each paragraph is one document.
corp <- corpus(
  paragraphs,
  docnames = paste0("para_", seq_along(paragraphs))
)

# Store the paragraph number as a document-level variable (docvar).
docvars(corp, "paragraph_number") <- seq_along(paragraphs)


# ═══════════════════════════════════════════════════════════════════════════════
# 3.  CORPUS SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n═══ CORPUS SUMMARY ═════════════════════════════════════════════════════════\n")
cat(sprintf("  Paragraphs  : %d\n", ndoc(corp)))
cat(sprintf("  Total tokens: %d\n", sum(ntoken(tokens(corp)))))
cat("\n  First 80 characters of each paragraph:\n\n")

for (i in seq_len(ndoc(corp))) {
  txt <- as.character(corp)[i]
  cat(sprintf("  [para_%02d] %s\n", i, str_trunc(txt, 80, ellipsis = "…")))
}

cat("════════════════════════════════════════════════════════════════════════════\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 4.  DEFINE THE DISTRIBUTIONAL IMPACT CODEBOOK
# ═══════════════════════════════════════════════════════════════════════════════
# The research question: when the IMF describes economic harm from the
# Middle East conflict, does it identify who bears disproportionate costs,
# through what transmission channel, and does it pair the observation with
# a policy remedy?
#
# qlm_codebook() takes:
#   name         – short label for the codebook
#   instructions – the task prompt the LLM sees before each paragraph
#   schema       – ellmer type_object() defining the output fields
#   role         – optional system-level role description
#   levels       – measurement level of each field ("nominal", "ordinal", etc.)
#                  used by qlm_compare() when computing inter-rater reliability
#
# NOTE on conditional NA: ellmer structured output cannot enforce "return NA
# when another field is FALSE" — that is a type constraint, not a logic one.
# Instead the descriptions tell the model to return the string "NA" for those
# fields when dist_impact is FALSE.  We convert those strings to real NA
# values in Task 2 after the coding run.
# ─────────────────────────────────────────────────────────────────────────────

dist_codebook <- qlm_codebook(
  
  name = "IMF WEO Distributional Impact",
  
  # The task instructions are shown to the LLM before every paragraph.
  instructions = paste(
    "You are coding paragraphs from the April 2026 IMF World Economic Outlook",
    "Executive Summary for a political science study of distributional impact.",
    "For each paragraph, assess whether the IMF identifies differential or",
    "disproportionate economic consequences across country groups, income levels,",
    "or regions — i.e., whether some actors are described as being hurt more",
    "than others by the Middle East conflict, trade barriers, or other shocks.",
    "Be conservative: code distributional impact as present only when the text",
    "draws an explicit or implicit contrast between groups, not when it describes",
    "harm to the global economy as a whole without differentiation.",
    "For fields marked 'Return the string NA if dist_impact is false', write",
    "exactly the two characters N and A (no quotes) when there is no",
    "distributional claim."
  ),
  
  # The role sets the LLM's persona at the system level.
  role = paste(
    "You are an expert political science research assistant with training in",
    "international political economy and discourse analysis of international",
    "financial institution communications."
  ),
  
  # schema defines the structured output the LLM must return for each paragraph.
  schema = type_object(
    
    # ── Field 1: dist_impact ───────────────────────────────────────────────────
    dist_impact = type_boolean(
      "Does this paragraph identify differential or disproportionate economic
      consequences across country groups, regions, or income levels?
      TRUE if the paragraph draws a contrast (explicit or implicit) between
      groups that are more vs. less affected.
      FALSE if the paragraph describes only aggregate global effects without
      distinguishing who bears more of the burden."
    ),
    
    # ── Field 2: affected_group ────────────────────────────────────────────────
    affected_group = type_string(
      "Which group does the IMF identify as disproportionately affected?
      Use the IMF's own language as closely as possible
      (e.g., 'emerging market and developing economies',
      'commodity-importing economies with preexisting fragilities',
      'the conflict region', 'the most vulnerable').
      Return the string NA if dist_impact is false."
    ),
    
    # ── Field 3: comparator_group ──────────────────────────────────────────────
    comparator_group = type_string(
      "Which group is the affected_group contrasted against?
      Use the IMF's own language as closely as possible
      (e.g., 'advanced economies', 'the global level', 'other regions').
      If no explicit comparator is named but the contrast is implied
      (e.g., 'more vulnerable economies' implies others are less vulnerable),
      write 'implied: [your brief inference]'.
      Return the string NA if dist_impact is false."
    ),
    
    # ── Field 4: transmission_channel ──────────────────────────────────────────
    transmission_channel = type_enum(
      c("commodity_prices", "financial_conditions", "trade",
        "fiscal_capacity", "institutional_weakness", "conflict_direct",
        "multiple", "unspecified", "none"),
      "Through what mechanism does the IMF say the disproportionate
      impact operates?
      'commodity_prices'       = oil, energy, food, or raw material prices;
      'financial_conditions'   = interest rates, capital flows, exchange rates,
                                 credit access, or dollar strength;
      'trade'                  = tariffs, trade barriers, supply chain disruption,
                                 or trade volumes;
      'fiscal_capacity'        = government budgets, debt levels, fiscal buffers,
                                 or inability to fund relief;
      'institutional_weakness' = weak governance, eroded institutions, or
                                 limited central bank credibility;
      'conflict_direct'        = physical destruction, displacement, or direct
                                 wartime economic disruption;
      'multiple'               = paragraph names two or more distinct channels;
      'unspecified'            = distributional claim present but no mechanism
                                 is identified;
      'none'                   = no distributional impact; use when dist_impact
                                 is false."
    ),
    
    # ── Field 5: magnitude ────────────────────────────────────────────────────
    magnitude = type_enum(
      c("quantified", "comparative", "unquantified", "none"),
      "How precisely does the IMF quantify the distributional asymmetry?
      'quantified'    = a specific number, percentage point, or ratio is given
                        (e.g., '0.3 percentage point', 'almost twice');
      'comparative'   = a relative comparison without exact numbers
                        (e.g., 'much more pronounced', 'more vulnerable');
      'unquantified'  = distributional claim present but no indication of
                        relative magnitude;
      'none'          = no distributional impact; use when dist_impact is false."
    ),
    
    # ── Field 6: remedy_paired ────────────────────────────────────────────────
    remedy_paired = type_boolean(
      "Does the same paragraph that identifies disproportionate harm also
      propose or reference a specific policy response directed at the
      disproportionately affected group?
      TRUE if a remedy is offered in the same paragraph (e.g., 'fiscal
      support should be targeted at the most vulnerable').
      FALSE if no remedy is mentioned, or if the paragraph only describes
      the asymmetry without prescribing action.
      Also FALSE if dist_impact is false."
    ),
    
    # ── Field 7: quoted_evidence ──────────────────────────────────────────────
    # Anchors each coding decision to the specific text that triggered it.
    quoted_evidence = type_string(
      "Copy the single sentence or shortest phrase from the paragraph that most
      directly supports your dist_impact and affected_group coding — use the
      exact words from the text, do not paraphrase.
      Return the string NA if dist_impact is false."
    )
    
  ), # end type_object / schema
  
  # Measurement levels inform qlm_compare() when computing inter-rater reliability.
  levels = list(
    dist_impact          = "nominal",
    affected_group       = "nominal",
    comparator_group     = "nominal",
    transmission_channel = "nominal",
    magnitude            = "ordinal",   # quantified > comparative > unquantified > none
    remedy_paired        = "nominal",
    quoted_evidence      = "nominal"
  )
  
) # end qlm_codebook()


# ═══════════════════════════════════════════════════════════════════════════════
# 5.  SAVE OBJECTS
# ═══════════════════════════════════════════════════════════════════════════════

saveRDS(corp,          file.path(wd, "imf_weo_corpus.rds"))
saveRDS(dist_codebook, file.path(wd, "dist_codebook.rds"))

cat("\nSaved to working directory:\n")
cat(sprintf("  %s\n", file.path(wd, "imf_weo_corpus.rds")))
cat(sprintf("  %s\n", file.path(wd, "dist_codebook.rds")))
cat("\nReady for Task 2 (LLM coding).\n")



# ═══════════════════════════════════════════════════════════════════════════════
# Task 2: LLM coding of distributional impact using qlm_code()
# ═══════════════════════════════════════════════════════════════════════════════



# ═══════════════════════════════════════════════════════════════════════════════
# 1.  LOAD SAVED OBJECTS FROM TASK 1
# ═══════════════════════════════════════════════════════════════════════════════

corp          <- readRDS(file.path(wd, "imf_weo_corpus.rds"))
dist_codebook <- readRDS(file.path(wd, "dist_codebook.rds"))

# Extract paragraphs as a named character vector.
# The names become the .id column in the qlm_code() output, which lets us
# join the coded results back to the corpus metadata afterwards.
para_texts <- as.character(corp)
names(para_texts) <- docnames(corp)   # "para_1", "para_2", ...


# ═══════════════════════════════════════════════════════════════════════════════
# 2.  RUN LLM CODING
# ═══════════════════════════════════════════════════════════════════════════════
# qlm_code() sends each paragraph to the model in turn and returns a tibble
# (qlm_coded object) with one row per paragraph and one column per schema field.
#
# model    – "openai/gpt-4o-mini" uses the ellmer OpenAI provider with that
#            model.  Your OPENAI_API_KEY environment variable must be set.
# name     – a label stored in the object's metadata for reproducibility
# notes    – free-text note also stored in metadata
# ─────────────────────────────────────────────────────────────────────────────

cat("Running LLM coding — this will make one API call per paragraph...\n")

coded <- qlm_code(
  x        = para_texts,
  codebook = dist_codebook,
  model    = "openai/gpt-4o-mini",
  name     = "imf_weo_apr2026_dist",
  notes    = "Distributional impact coding, April 2026 WEO Executive Summary"
)

cat("Done.\n\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 3.  POST-PROCESS THE CODED OUTPUT
# ═══════════════════════════════════════════════════════════════════════════════
# The LLM was instructed to return the string "NA" for affected_group,
# comparator_group, and quoted_evidence when dist_impact is FALSE (because
# ellmer's type_string() cannot be made truly nullable).  We convert those
# strings to proper R NA here.
#
# We also join the paragraph_number docvar from the corpus so the final data
# frame has an integer ID column for merging with other data later.
# ─────────────────────────────────────────────────────────────────────────────

# 3a. Convert "NA" strings → real NA in the free-text fields.
coded_clean <- coded |>
  mutate(
    across(
      c(affected_group, comparator_group, quoted_evidence),
      ~ ifelse(. == "NA", NA_character_, .)
    )
  )

# 3b. Pull paragraph metadata (number) from the corpus docvars and join on.
para_meta <- docvars(corp) |>
  mutate(doc_id = docnames(corp))   # creates a "para_1", "para_2", ... key

coded_clean <- coded_clean |>
  left_join(para_meta, by = c(".id" = "doc_id"))

# 3c. Reorder columns so identifiers come first.
coded_clean <- coded_clean |>
  select(paragraph_number, .id, dist_impact, affected_group, comparator_group,
         transmission_channel, magnitude, remedy_paired, quoted_evidence)


# ═══════════════════════════════════════════════════════════════════════════════
# 4.  PRINT SUMMARY TABLE
# ═══════════════════════════════════════════════════════════════════════════════

cat("═══ CODING RESULTS ══════════════════════════════════════════════════════════\n\n")

# Count of distributional-impact paragraphs
n_dist <- sum(coded_clean$dist_impact, na.rm = TRUE)
cat(sprintf("  Paragraphs coded              : %d\n", nrow(coded_clean)))
cat(sprintf("  Distributional impact (TRUE)  : %d\n", n_dist))
cat(sprintf("  No distributional claim (FALSE): %d\n", nrow(coded_clean) - n_dist))

# Transmission channel breakdown
cat("\n  Transmission channel breakdown:\n")
print(table(coded_clean$transmission_channel, useNA = "ifany"))

# Magnitude breakdown
cat("\n  Magnitude breakdown:\n")
print(table(coded_clean$magnitude, useNA = "ifany"))

# Remedy paired breakdown
cat("\n  Remedy paired with distributional claim:\n")
print(table(coded_clean$remedy_paired, useNA = "ifany"))

# Row-by-row detail
cat("\n  Row-by-row detail:\n\n")
for (i in seq_len(nrow(coded_clean))) {
  row <- coded_clean[i, ]
  cat(sprintf("  [para_%02d] dist=%-5s | %-20s | mag=%-13s | remedy=%-5s\n",
              row$paragraph_number,
              row$dist_impact,
              ifelse(is.na(row$affected_group), "(none)",
                     str_trunc(row$affected_group, 20, ellipsis = "…")),
              row$magnitude,
              row$remedy_paired))
}

cat("\n════════════════════════════════════════════════════════════════════════════\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 5.  SAVE RESULTS
# ═══════════════════════════════════════════════════════════════════════════════
# Save two versions:
#   - the raw qlm_coded object (retains quallmer metadata for qlm_compare() etc.)
#   - the cleaned data frame (easy to work with in downstream analysis)

saveRDS(coded,       file.path(wd, "imf_weo_dist_coded_raw.rds"))
saveRDS(coded_clean, file.path(wd, "imf_weo_dist_coded.rds"))

cat("\nSaved:\n")
cat(sprintf("  %s\n", file.path(wd, "imf_weo_dist_coded_raw.rds")))
cat(sprintf("  %s\n", file.path(wd, "imf_weo_dist_coded.rds")))
cat("\nTo inspect full results: View(coded_clean)\n")

View(coded_clean)