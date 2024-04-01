package com.dpw.runner.shipment.services.validator.constants;

public class ErrorConstants {
    private ErrorConstants(){}

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
}