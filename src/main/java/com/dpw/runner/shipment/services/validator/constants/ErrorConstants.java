package com.dpw.runner.shipment.services.validator.constants;

public class ErrorConstants {
    public static final String INVALID_FIELD_TYPE_VALIDATION = "Invalid field type for field: %s, given field type: %s but expected type was: %s.";
    public static final String INVALID_COMPARISION_VALIDATION = "Comparison validation failed between field %s and %s.";
    public static final String INVALID_REQUIRED_FIELD_VALIDATION = "Required field validation failed for field: %s.";
    public static final String INVALID_PATTERN_VALIDATION = "Regex pattern validation failed for field: %s.";
    public static final String INVALID_MIN_SIZE_VALIDATION = "Invalid minSize for field: %s, given size is: %s, but expected minSize was: %s.";
    public static final String INVALID_MAX_SIZE_VALIDATION = "Invalid maxSize for field: %s, given size is: %s, but expected maxSize was: %s.";
    public static final String INVALID_MIN_VALUE_VALIDATION = "Invalid minValue for field: %s, given value is: %s, but expected minValue was: %s.";
    public static final String INVALID_MAX_VALUE_VALIDATION = "Invalid maxValue for field: %s, given value is: %s, but expected maxValue was: %s.";
    public static final String INVALID_ENUM_VALIDATION = "Enum validation failed for field: %s with value provided: %s. Supported values are: %s.";
    public static final String INVALID_CONDITIONAL_COMPARISON = "No Conditional comparison met for field: %s, please check dependent data.";
    public static final String INVALID_UNIQUE_CONSTRAINT = "Unique constraint failed for property: %s in field: %s.";
    public static final String LOCK_UNLOCK_ERROR = "%s is Locked by User %s. Please request to Unlock for further update.";
    public static final String HAWB_NOT_GENERATED_ERROR = "Original HAWB is not generated for the shipment %s. Please generate them before printing the Original MAWB.";
    public static final String VALIDATE_INTER_BRANCH_CONSOLE = "This consolidation has inter branch shipments attached to it, please detach those shipments before disabling inter branch consolidation.";
    public static final String ERROR_WHILE_EMAIL = "Error occurred during sending email: with exception: %s";
    public static final String ERROR_WHILE_SYNC = "Error occurred during Sync: with exception: %s";
    public static final String VALIDATE_JOB_TYPE_CHANGE = "Pull/ Push request is already in progress, Cannot change Consolidation Type.";
    public static final String INVALID_TRANSPORT_MODE = "Selected Transport Mode %s is not allowed.";
    public static final String ERROR_WHILE_CREATING_EVENT = "Error occurred while creating event: with exception: %s";
    private ErrorConstants() {
    }
}