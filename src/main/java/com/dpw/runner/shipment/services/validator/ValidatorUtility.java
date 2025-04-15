package com.dpw.runner.shipment.services.validator;

import com.dpw.runner.shipment.services.dao.interfaces.IValidationsDao;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.constants.ErrorConstants;
import com.dpw.runner.shipment.services.validator.constants.ValidatorConstants;
import com.dpw.runner.shipment.services.validator.enums.JsonTypes;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.json.*;
import java.io.StringReader;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ValidatorUtility {

    private final ObjectMapper objectMapper;
    private final IValidationsDao validationsDao;

    private static Map<String, List<Validations>> validationsMap = new HashMap();
    @Autowired
    public ValidatorUtility(ObjectMapper objectMapper, IValidationsDao validationsDao) {
        this.objectMapper = objectMapper;
        this.validationsDao = validationsDao;
    }


    /**
     * * This method would be called while applying any validation
     * @param json
     * @param entity
     * @param lifecycleHook
     */

    public Set<String> applyValidation(String json, String entity, LifecycleHooks lifecycleHook, boolean failOnFirst) {
        Set<String> errors = new LinkedHashSet<>();
        Map jsonMap = new HashMap();
        long start = System.currentTimeMillis();
        log.info("Initiating Validation Layer with for entity: {} raw data: {}", entity, json);
        try (JsonReader jsonReader = Json.createReader(new StringReader(json))) {
            JsonObject jsonObject = jsonReader.readObject();
            generateMap(jsonObject, StringUtility.getEmptyString(), jsonMap);
            String key = String.format("%s-%s", entity, lifecycleHook);

            if (!validationsMap.containsKey(key))
                validationsMap.put(key, validationsDao.findByLifecycleHookAndEntity(lifecycleHook, entity).get());
            for (Validations validation : validationsMap.get(key)) {
                try {
                    try (JsonReader schemaReader = Json.createReader(new StringReader(objectMapper.writeValueAsString(validation.getJsonSchema())))) {
                        JsonObject schemaObject = schemaReader.readObject();
                        errors.addAll(validateJson(jsonObject, schemaObject, jsonMap, failOnFirst));
                    }
                } catch (JsonProcessingException e) {
                    throw new RuntimeException(e);
                }
            }
            log.info("Ending Validation Layer with for entity: {} with time taken: {} ms", entity, System.currentTimeMillis() - start);
        }

        return errors;
    }

    private Set<String> validateJson(JsonObject jsonObject, JsonObject schemaObject, Map<String, Object> jsonMap, boolean failOnFirst) {
        try {
            return validateFields(jsonObject, schemaObject.getJsonObject(ValidatorConstants.PROPERTIES), jsonMap, failOnFirst);
        }
        catch (Exception ex) {
            log.error("Validation failed due to {}", ex.getMessage());
            ex.printStackTrace();
            throw ex;
        }
    }

    private void generateMap(JsonObject jsonObject, String prefix, Map jsonMap) {
        for (String key : jsonObject.keySet()) {
            if (jsonObject.get(key).getValueType() == JsonValue.ValueType.OBJECT) {
                generateMap(jsonObject.getJsonObject(key), key + ".", jsonMap);
            } else {
                jsonMap.put(prefix + key, jsonObject.get(key));
            }
        }
    }


    private Set<String> validateFields(JsonObject jsonObject, JsonObject schemaObject, Map<String, Object> jsonMap, boolean failOnFirst) {
        Set<String> errors = new LinkedHashSet<String>();
        if (schemaObject == null) {
            return errors;
        }
        for (String field : schemaObject.keySet()) {
            JsonObject fieldSchema = schemaObject.getJsonObject(field);

            for (String validationProperty : fieldSchema.keySet()) {
                switch (validationProperty) {

                    case ValidatorConstants.PROPERTIES:
                        errors.addAll(validateFields(jsonObject.getJsonObject(field), fieldSchema.getJsonObject(ValidatorConstants.PROPERTIES), jsonMap, failOnFirst));
                        break;

                    case ValidatorConstants.REQUIRED:
                        errors.addAll(validateRequired(jsonObject, schemaObject.getJsonObject(field), field));
                        break;

                    case ValidatorConstants.PATTERN:
                        errors.addAll(validatePattern(jsonObject, schemaObject.getJsonObject(field), field));
                        break;

                    case ValidatorConstants.MIN_SIZE:
                        errors.addAll(validateMinSize(jsonObject, schemaObject.getJsonObject(field), field));
                        break;

                    case ValidatorConstants.MAX_SIZE:
                        errors.addAll(validateMaxSize(jsonObject, schemaObject.getJsonObject(field), field));
                        break;

                    case ValidatorConstants.MIN_VALUE:
                        errors.addAll(validateMinValue(jsonObject, schemaObject.getJsonObject(field), field));
                        break;

                    case ValidatorConstants.MAX_VALUE:
                        errors.addAll(validateMaxValue(jsonObject, schemaObject.getJsonObject(field), field));
                        break;

                    case ValidatorConstants.ENUM:
                        errors.addAll(validateEnum(jsonObject, schemaObject.getJsonObject(field), field));
                        break;

                    case ValidatorConstants.TYPE:
                        errors.addAll(validateType(jsonObject, schemaObject.getJsonObject(field), field));
                        break;

                    case ValidatorConstants.COMPARE:
                        errors.addAll(validateCompare(jsonObject, schemaObject.getJsonObject(field), field, jsonMap));
                        break;

                    case ValidatorConstants.CONDITIONAL_COMPARE:
                        errors.addAll(validateConditionalCompare(jsonObject, schemaObject.getJsonObject(field), field, jsonMap));
                        break;

                    case ValidatorConstants.ARRAY_PROPERTIES:
                        errors.addAll(validateArrayProperties(jsonObject, fieldSchema.getJsonObject(ValidatorConstants.ARRAY_PROPERTIES), field, jsonMap));
                        break;
                    default:
                }

                /** Whenever fails-on-first will be enabled, rest of the validations will not be checked */
                if (failOnFirst && ! errors.isEmpty())
                    return errors;
            }
        }

        return errors;
    }

    private Set<String> validateRequired(JsonObject jsonObject, JsonObject jsonSchema, String at) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (jsonSchema.containsKey(ValidatorConstants.REQUIRED) && jsonSchema.getBoolean(ValidatorConstants.REQUIRED)
                && (fieldValue == null || fieldValue.getValueType() == JsonValue.ValueType.NULL || (fieldValue.getValueType() == JsonValue.ValueType.STRING && StringUtility.isEmpty(jsonObject.getString(at)))) ) {
            errors.add(String.format(ErrorConstants.INVALID_REQUIRED_FIELD_VALIDATION, at));
        }
        return errors;
    }

    private Set<String> validatePattern(JsonObject jsonObject, JsonObject jsonSchema, String at) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (fieldValue != null) {
            String pattern = jsonSchema.getString(ValidatorConstants.PATTERN);
            String error = getErrorMessage(jsonSchema.getJsonObject(ValidatorConstants.ERRORS), ValidatorConstants.PATTERN);
            if (fieldValue.getValueType() == JsonValue.ValueType.STRING) {
                String fieldValueString = jsonObject.getString(at);
                if (StringUtility.isNotEmpty(fieldValueString) && !Pattern.matches(pattern, fieldValueString)) {
                    errors.add(StringUtility.isEmpty(error) ? String.format(ErrorConstants.INVALID_PATTERN_VALIDATION, at) : error);
                }
            }
        }
        return errors;
    }

    private Set<String> validateMinSize(JsonObject jsonObject, JsonObject jsonSchema, String at) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (fieldValue != null) {
            Integer size = jsonSchema.getInt(ValidatorConstants.MIN_SIZE);
            switch (fieldValue.getValueType()) {
                case STRING:
                    String fieldValueString = jsonObject.getString(at);
                    if (fieldValueString != null && size != null && fieldValueString.length() < size) {
                        errors.add(String.format(ErrorConstants.INVALID_MIN_SIZE_VALIDATION, at, fieldValueString.length(), size));
                    }
                    break;

                case ARRAY:
                    JsonArray fieldValueArray = fieldValue.asJsonArray();
                    if (fieldValueArray != null && size != null && fieldValueArray.size() < size) {
                        errors.add(String.format(ErrorConstants.INVALID_MIN_SIZE_VALIDATION, at, fieldValueArray.size(), size));
                    }
                    break;
                default:
            }
        }

        return errors;
    }

    private Set<String> validateMaxSize(JsonObject jsonObject, JsonObject jsonSchema, String at) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (fieldValue != null) {
            Integer size = jsonSchema.getInt(ValidatorConstants.MAX_SIZE);
            switch (fieldValue.getValueType()) {
                case STRING:
                    String fieldValueString = jsonObject.getString(at);
                    if (fieldValueString != null && size != null && fieldValueString.length() > size) {
                        errors.add(String.format(ErrorConstants.INVALID_MAX_SIZE_VALIDATION, at, fieldValueString.length(), size));

                    }
                    break;

                case ARRAY:
                    JsonArray fieldValueArray = fieldValue.asJsonArray();
                    if (fieldValueArray != null && size != null && fieldValueArray.size() > size) {
                        errors.add(String.format(ErrorConstants.INVALID_MAX_SIZE_VALIDATION, at, fieldValueArray.size(), size));

                    }
                    break;
                default:
            }
        }

        return errors;
    }

    private Set<String> validateMinValue(JsonObject jsonObject, JsonObject jsonSchema, String at) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (fieldValue != null) {

            if (fieldValue.getValueType() == JsonValue.ValueType.NUMBER) {
                Integer fieldValueInteger = jsonObject.getInt(at);
                Integer size = jsonSchema.getInt(ValidatorConstants.MIN_VALUE);
                if (fieldValueInteger != null && fieldValueInteger < size) {
                    errors.add(String.format(ErrorConstants.INVALID_MIN_VALUE_VALIDATION, at, fieldValueInteger, size));
                }
            }
        }

        return errors;
    }

    private Set<String> validateMaxValue(JsonObject jsonObject, JsonObject jsonSchema, String at) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (fieldValue != null) {

            if (fieldValue.getValueType() == JsonValue.ValueType.NUMBER) {
                Integer fieldValueInteger = jsonObject.getInt(at);
                Integer size = jsonSchema.getInt(ValidatorConstants.MAX_VALUE);
                if (fieldValueInteger != null && fieldValueInteger > size) {
                    errors.add(String.format(ErrorConstants.INVALID_MAX_VALUE_VALIDATION, at, fieldValueInteger, size));
                }
            }
        }

        return errors;
    }


    private  Set<String> validateEnum(JsonObject jsonObject, JsonObject jsonSchema, String at) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (fieldValue != null) {

            JsonArray enumList = jsonSchema.getJsonArray(ValidatorConstants.ENUM);
            switch (fieldValue.getValueType()) {
                case STRING:
                    Set<String> enumStringList = enumList.stream().map(c -> c.toString().replace("\"", "")).collect(Collectors.toSet());
                    if (!enumStringList.isEmpty() && !enumStringList.contains(jsonObject.getString(at)))
                        errors.add(String.format(ErrorConstants.INVALID_ENUM_VALIDATION, at, jsonObject.getString(at), enumStringList));
                    break;

                case NUMBER:
                    Integer fieldValueInteger = jsonObject.getInt(at);
                    Set enumIntegerList = enumList.stream().map(c -> Integer.parseInt(c.toString())).collect(Collectors.toSet());
                    if (!enumIntegerList.isEmpty() && !enumIntegerList.contains(fieldValueInteger))
                        errors.add(String.format(ErrorConstants.INVALID_ENUM_VALIDATION, at, fieldValueInteger, enumIntegerList));

                    break;
                default:
            }
        }
        return errors;
    }

    private Set<String> validateType(JsonObject jsonObject, JsonObject jsonSchema, String at) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (fieldValue != null) {
            JsonTypes schemaValue = JsonTypes.fromValue(jsonSchema.getString(ValidatorConstants.TYPE));

            switch (schemaValue) {
                case OBJECT:
                    if (fieldValue.getValueType() != JsonValue.ValueType.OBJECT)
                        errors.add(String.format(ErrorConstants.INVALID_FIELD_TYPE_VALIDATION, at, fieldValue.getValueType(), schemaValue));
                    break;

                case ARRAY:
                    if (fieldValue.getValueType() != JsonValue.ValueType.ARRAY)
                        errors.add(String.format(ErrorConstants.INVALID_FIELD_TYPE_VALIDATION, at, fieldValue.getValueType(), schemaValue));
                    break;

                case STRING:
                    if (fieldValue.getValueType() != JsonValue.ValueType.STRING)
                        errors.add(String.format(ErrorConstants.INVALID_FIELD_TYPE_VALIDATION, at, fieldValue.getValueType(), schemaValue));
                    break;

                case NUMBER:
                    if (fieldValue.getValueType() != JsonValue.ValueType.NUMBER)
                        errors.add(String.format(ErrorConstants.INVALID_FIELD_TYPE_VALIDATION, at, fieldValue.getValueType(), schemaValue));
                    break;

                case DATE:
                    if (! isValidaDate(jsonObject.getString(at)))
                        errors.add(String.format(ErrorConstants.INVALID_FIELD_TYPE_VALIDATION, at, fieldValue.getValueType(), schemaValue));
                    break;

                case DATE_TIME:
                   if (! isValidaDateTime(jsonObject.getString(at)))
                       errors.add(String.format(ErrorConstants.INVALID_FIELD_TYPE_VALIDATION, at, fieldValue.getValueType(), schemaValue));
                    break;

                case BOOLEAN:
                    if (fieldValue.getValueType() != JsonValue.ValueType.TRUE && fieldValue.getValueType() != JsonValue.ValueType.FALSE)
                        errors.add(String.format(ErrorConstants.INVALID_FIELD_TYPE_VALIDATION, at, fieldValue.getValueType(), schemaValue));
                    break;

                case NULL:
                    if (fieldValue.getValueType() != JsonValue.ValueType.NULL)
                        errors.add(String.format(ErrorConstants.INVALID_FIELD_TYPE_VALIDATION, at, fieldValue.getValueType(), schemaValue));
                    break;

                default:
            }
        }
        return errors;
    }


    private Set<String> validateCompare(JsonObject jsonObject, JsonObject jsonSchema, String at, Map jsonMap) {
        Set<String> errors = new LinkedHashSet();
        JsonValue fieldValue = jsonObject.get(at);
        if (fieldValue != null) {
            JsonArray compareList = jsonSchema.getJsonArray(ValidatorConstants.COMPARE);
            errors.addAll(validateCompare(compareList, jsonObject, at, jsonMap));
        }

        return errors;
    }

    private Set<String> validateCompare(JsonArray schemaArray, JsonObject jsonObject, String at, Map<String, Object> jsonMap) {
        Set<String> errors = new LinkedHashSet();

        JsonValue currentValue = jsonObject.get(at);
        for (JsonValue compare : schemaArray) {
            JsonObject compareWithJson = (JsonObject) compare;

            if (compareWithJson.containsKey(ValidatorConstants.COMPARE_TO) && compareWithJson.containsKey(ValidatorConstants.OPERATOR)) {
                String compareWith = compareWithJson.getString(ValidatorConstants.COMPARE_TO);
                Operators operator = Operators.fromValue(compareWithJson.getString(ValidatorConstants.OPERATOR));
                switch (operator) {

                    case EQUALS:
                        switch (currentValue.getValueType()) {
                            // For comparing String & Date-time, behaviour would be similar as both are coming as type string and comparison process is same.
                            case STRING:
                                if (!jsonMap.containsKey(compareWith) || !jsonMap.get(compareWith).toString().equalsIgnoreCase(jsonObject.getString(at)))
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                break;

                            case NUMBER:
                                if (!jsonMap.containsKey(compareWith) || ((Integer) jsonMap.get(compareWith) != jsonObject.getInt(at)))
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                break;

                            case TRUE:

                            case FALSE:
                                if (! jsonMap.containsKey(compareWith) || ((Boolean) jsonMap.get(compareWith) != jsonObject.getBoolean(at)))
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                break;
                            default:

                        }
                        break;

                    case NOT_EQUALS:
                        switch (currentValue.getValueType()) {
                            // For comparing String & Date-time, behaviour would be similar as both are coming as type string and comparison process is same.
                            case STRING:
                                if (!jsonMap.containsKey(compareWith) || jsonMap.get(compareWith).toString().equalsIgnoreCase(jsonObject.getString(at)))
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                break;

                            case NUMBER:
                                if (!jsonMap.containsKey(compareWith) || ((Integer) jsonMap.get(compareWith) == jsonObject.getInt(at)))
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                break;

                            case TRUE:

                            case FALSE:
                                if (!jsonMap.containsKey(compareWith) || ((Boolean) jsonMap.get(compareWith) == jsonObject.getBoolean(at)))
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                break;
                            default:
                        }
                        break;

                    case LESSER_THAN:
                        switch (currentValue.getValueType()) {
                            case NUMBER:
                                if (!jsonMap.containsKey(compareWith) || jsonObject.getInt(at) >= ((Integer) jsonMap.get(compareWith)))
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                break;

                            // Comparison of Date-time fields
                            case STRING:
                                if (!jsonMap.containsKey(compareWith) && isValidaDateTime(jsonObject.getString(at)) && isValidaDateTime(jsonObject.getString(compareWith))
                                        && !isValidDateComparison(jsonObject.getString(at), jsonObject.getString(compareWith), Operators.LESSER_THAN)) {
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                }
                                break;
                            default:
                        }
                        break;

                    case LESSER_THAN_EQUALS:
                        switch (currentValue.getValueType()) {
                            case NUMBER:
                                if (!jsonMap.containsKey(compareWith) || jsonObject.getInt(at) > ((Integer) jsonMap.get(compareWith))) {
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                }
                                break;

                            // Comparison of Date-time fields
                            case STRING:
                                if (jsonMap.containsKey(compareWith) && isValidaDateTime(jsonObject.getString(at)) && isValidaDateTime(String.valueOf(jsonMap.get(compareWith)))
                                        && !isValidDateComparison(jsonObject.getString(at), String.valueOf(jsonMap.get(compareWith)), Operators.LESSER_THAN_EQUALS)) {
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                }
                                break;
                            default:
                        }
                        break;

                    case GREATER_THAN:
                        switch (currentValue.getValueType()) {
                            case NUMBER:
                                if (!jsonMap.containsKey(compareWith) || jsonObject.getInt(at) <= ((Integer) jsonMap.get(compareWith))) {
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                }
                                break;

                            // Comparison of Date-time fields
                            case STRING:
                                if (jsonMap.containsKey(compareWith) && isValidaDateTime(jsonObject.getString(at)) && isValidaDateTime(jsonMap.get(compareWith).toString())
                                        && !isValidDateComparison(jsonObject.getString(at), String.valueOf(jsonMap.get(compareWith)), Operators.GREATER_THAN)) {
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                }
                                break;
                            default:
                        }
                        break;

                    case GREATER_THAN_EQUALS:
                        switch (currentValue.getValueType()) {
                            case NUMBER:
                                if (!jsonMap.containsKey(compareWith) || jsonObject.getInt(at) < ((Integer) jsonMap.get(compareWith))) {
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                }
                                break;

                            // Comparison of Date-time fields
                            case STRING:
                                if (jsonMap.containsKey(compareWith) && isValidaDateTime(jsonObject.getString(at)) && isValidaDateTime(StringUtility.convertToString(jsonMap.get(compareWith)))
                                        && !isValidDateComparison(jsonObject.getString(at), String.valueOf(jsonMap.get(compareWith)), Operators.GREATER_THAN_EQUALS)) {
                                    errors.add(String.format(ErrorConstants.INVALID_COMPARISION_VALIDATION, at, compareWith));
                                }
                                break;
                            default:
                        }
                        break;
                    default:
                }
            }
        }

        return errors;
    }

    private boolean isValidDateComparison(String value, String compareTo, Operators operators) {
        LocalDateTime valueDate = LocalDateTime.parse(value.replace("\"", ""));
        LocalDateTime compareToData = LocalDateTime.parse(compareTo.replace("\"", ""));
        int compare = valueDate.compareTo(compareToData);

        switch (operators) {
            case EQUALS:
                return compare == 0;
            case NOT_EQUALS:
                return compare != 0;
            case LESSER_THAN:
                return compare < 0;
            case LESSER_THAN_EQUALS:
                return compare <= 0;
            case GREATER_THAN:
                return compare > 0;
            case GREATER_THAN_EQUALS:
                return compare >= 0;
            default:
        }
        return true;
    }

    private boolean isValidaDateTime(String date) {
        try {
            LocalDateTime.parse(date.replace("\"", ""));
        } catch (Exception ex) {
            return false;
        }
        return true;
    }

    private boolean isValidaDate(String date) {
        try {
            LocalDate.parse(date.replace("\"", ""));
        } catch (Exception ex) {
            return false;
        }
        return true;
    }

    /**
     * Validating conditional-comparison
     * Supported validations:
     *      1. Data-type supported: String, number & date
     *      2. Operations supported: Equals, not-equals & in
     */
    private Set<String> validateConditionalCompare(JsonObject jsonObject, JsonObject jsonSchema, String at, Map<String, Object> jsonMap) {
        Set<String> errors = new LinkedHashSet();
        JsonArray conditionalArray = jsonSchema.getJsonArray(ValidatorConstants.CONDITIONAL_COMPARE);
        JsonValue fieldValue = jsonObject.get(at);
        Boolean isValid = false;
        for (JsonValue _schema : conditionalArray) {
            JsonObject schema = _schema.asJsonObject();
            JsonArray compare = schema.getJsonArray(ValidatorConstants.COMPARE);
            switch (fieldValue.getValueType()) {
                case STRING:
                    String fieldValueString = jsonObject.getString(at);
                    if (StringUtility.isNotEmpty(fieldValueString) && fieldValueString.equals(schema.getString(ValidatorConstants.VALUE)))
                        isValid = validateAdditionalCompare(jsonObject, compare, at, jsonMap);
                    break;

                case NUMBER:
                    Integer fieldValueInteger = jsonObject.getInt(at);
                    if (fieldValueInteger != null && fieldValueInteger == schema.getInt(ValidatorConstants.VALUE))
                        isValid = validateAdditionalCompare(jsonObject, compare, at, jsonMap);
                    break;
                default:
            }

            if (isValid) {
                return errors;
            }
        }
        errors.add(String.format(ErrorConstants.INVALID_CONDITIONAL_COMPARISON, at));
        return errors;
    }

    private Boolean validateAdditionalCompare(JsonObject jsonObject, JsonArray jsonArray, String at, Map jsonMap) {
        for (JsonValue current : jsonArray) {
            JsonObject schemaObject = (JsonObject) current;
            if (schemaObject.containsKey(ValidatorConstants.COMPARE_TO) && schemaObject.containsKey(ValidatorConstants.OPERATOR) && schemaObject.containsKey(ValidatorConstants.VALUE)) {
                String compareWith = schemaObject.getString(ValidatorConstants.COMPARE_TO);
                JsonValue value = schemaObject.get(ValidatorConstants.VALUE);
                JsonValue compareWithValue = (JsonValue) jsonMap.get(compareWith);
                if (compareWithValue != null) {
                    Operators operator = Operators.fromValue(schemaObject.getString(ValidatorConstants.OPERATOR));

                    switch (compareWithValue.getValueType()) {
                        case STRING:
                            String fieldValueString = String.valueOf(jsonMap.get(compareWith)).replace("\"", "");
                            switch (operator) {
                                case IN:
                                    JsonArray enumList = schemaObject.getJsonArray(ValidatorConstants.VALUE);
                                    Set<String> enumStringList = enumList.stream().map(c -> c.toString().replace("\"", "")).collect(Collectors.toSet());
                                    if (!enumStringList.isEmpty() && !enumStringList.contains(fieldValueString))
                                        return false;
                                    break;

                                case EQUALS:
                                    String compareToValue = schemaObject.getString(ValidatorConstants.VALUE);
                                    if (!fieldValueString.equals(compareToValue))
                                        return false;
                                    break;

                                case NOT_EQUALS:
                                    String compareToValue1 = schemaObject.getString(ValidatorConstants.VALUE);
                                    if (fieldValueString.equals(compareToValue1))
                                        return false;
                                    break;
                                default:
                            }
                            break;

                        case NUMBER:
                            Integer fieldValueInteger = Integer.parseInt(String.valueOf(jsonMap.get(compareWith)));
                            switch (operator) {
                                case IN:
                                    JsonArray enumList = schemaObject.getJsonArray(ValidatorConstants.VALUE);
                                    Set enumIntegerList = enumList.stream().map(c -> Integer.parseInt(c.toString())).collect(Collectors.toSet());
                                    if (!enumIntegerList.isEmpty() && !enumIntegerList.contains(fieldValueInteger))
                                        return false;

                                    break;
                                case EQUALS:
                                    Integer compareToValue = schemaObject.getInt(ValidatorConstants.VALUE);
                                    if (compareToValue != fieldValueInteger)
                                        return false;
                                    break;

                                case NOT_EQUALS:
                                    Integer compareToValue1 = schemaObject.getInt(ValidatorConstants.VALUE);
                                    if (compareToValue1 == fieldValueInteger)
                                        return false;
                                    break;
                                default:
                            }
                            break;
                        default:
                    }
                }

            }

        }

        return true;
    }

    /**
     * Validating array-properties
     * Supported validations:
     *      1. Array: Type, Min-size, Max-size, Unique (property) * *
     *      2. Array properties: All root level field validation*
     */
    private Set<String> validateArrayProperties(JsonObject jsonObject, JsonObject jsonSchema, String at, Map jsonMap) {
        Set<String> errors = new LinkedHashSet();
        JsonArray fieldValueArray = jsonObject.getJsonArray(at);

        for (String key : jsonSchema.keySet()) {
            switch (key) {

                case ValidatorConstants.PROPERTIES:
                    for (JsonValue value : fieldValueArray)
                        errors.addAll(validateFields(value.asJsonObject(), jsonSchema.getJsonObject(ValidatorConstants.PROPERTIES), jsonMap, false));
                    break;

                case ValidatorConstants.UNIQUE:
                    for (JsonValue value :  jsonSchema.getJsonArray(ValidatorConstants.UNIQUE)) {
                        String uniqueKey = value.toString().replace("\"", "");
                        Set<JsonValue> set = new HashSet<>();

                        for (JsonValue currentValue : fieldValueArray) {
                            JsonValue _value = currentValue.asJsonObject().get(uniqueKey);

                            if (_value != null && set.contains(_value))
                                errors.add(String.format(ErrorConstants.INVALID_UNIQUE_CONSTRAINT, uniqueKey, at));
                            set.add(_value);

                        }
                    }
                    break;
                default:
            }

        }
        return errors;
    }

    private String getErrorMessage(JsonObject errorJson, String key) {
        String error = null;
        if (Objects.isNull(errorJson) || Objects.isNull(key))
            return null;
        if (errorJson.containsKey(key) && errorJson.get(key).getValueType() == JsonValue.ValueType.STRING)
            error = errorJson.getString(key);
        return error;
    }

    public void clearValidationsMap() {
        validationsMap.clear();
    }

}
