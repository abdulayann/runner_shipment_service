package com.dpw.runner.shipment.services.validator;

import com.dpw.runner.shipment.services.dao.interfaces.IValidationsDao;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;
import java.io.StringReader;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ValidatorUtilityTest {

    @InjectMocks
    private ValidatorUtility validatorUtility;

    @Mock
    private IValidationsDao validationsDao;

    @Mock
    private ObjectMapper objectMapper;


    @Test
    void testApplyValidation() throws Exception {
        String json2 = "{\"name\": \"John Doe\", \"age\": 30, \"email\": \"john.doe@example.com\", \"password\": \"password123\", \"confirmPassword\": \"password123\", \"roles\": [\"admin\", \"user\"], \"settings\": {\"theme\": \"dark\", \"notifications\": true}," + "\"date\": 2024-06-03, \"dateTime\": 2024-06-03T12:00:00, \"boolean\": true, \"null\": null}";
        String json = "{\"name\": \"John Doe\", \"invalidName\": 223 , \"age\": 30, \"email\": \"john.doe@example.com\", \"password\": \"password123\", \"confirmPassword\": \"password123\", \"roles\": [\"admin\", \"user\"], \"settings\": {\"theme\": \"dark\", \"notifications\": true}, \"date\": \"2024-06-03\", \"invalidDate\": \"2024-26-23\", \"dateTime\": \"2024-06-03T12:00:00\", \"boolean\": true, \"invalidBoolean\": \"true\", \"null\": null}";

        String schema = "{\"properties\": {\"invalidBoolean\" : {\"type\": \"boolean\"}, \"invalidDate\" : {\"type\" : \"date\"}, \"date\" : {\"type\" : \"date\"}, \"invalidName\": {\"type\": \"string\"}, \"name\": {\"type\": \"string\", \"minSize\": 3, \"maxSize\": 50, \"required\": true}, \"age\": {\"type\": \"number\", \"minValue\": 18, \"maxValue\": 100, \"required\": true}, \"email\": {\"type\": \"string\", \"pattern\": \"^[^@\\\\s]+@[^@\\\\s]+\\\\.[^@\\\\s]+$\", \"required\": true}, \"password\": {\"type\": \"string\", \"minSize\": 8, \"required\": true}, \"confirmPassword\": {\"type\": \"string\", \"compare\": [{\"compareTo\":\"carrierDetails.etd\",\"operator\":\"equals\"}], \"required\": true}, \"roles\": {\"type\": \"array\", \"minSize\": 1, \"enum\": [\"admin\", \"user\", \"guest\"], \"arrayProperties\": {\"type\": \"string\"}, \"required\": true}, \"settings\": {\"type\": \"object\", \"properties\": {\"theme\": {\"type\": \"string\", \"enum\": [\"light\", \"dark\"], \"required\": true}, \"notifications\": {\"type\": \"boolean\", \"required\": false}}, \"required\": true}}}";

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(4, errors.size());
    }

    @Test
    void testApplyValidation_testValidateCompare() throws Exception {
        String json = "{\"name\": \"123\"}";
        String schema =
        "{\"properties\": {\"name\" : {\"type\": \"string\", \"compare\": [{\"compareTo\":\"23\",\"operator\":\"not-equals\"}, {\"compareTo\":\"1\",\"operator\":\"lesser-than\"}, {\"compareTo\":\"1\",\"operator\":\"greater-than\"}, {\"compareTo\":\"1\",\"operator\":\"lesser-than-equals\"}, {\"compareTo\":\"1\",\"operator\":\"greater-than-equals\"}]}}}";

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(1, errors.size());
    }

    @Test
    void testApplyValidation_testValidateCompare2() throws Exception {
        String json = "{\"name\": \"123\"}";
        String schema =
                "{\"properties\": {\"name\" : {\"type\": \"string\", \"compare\": [{\"compareTo\":\"1\",\"operator\":\"lesser-than\"}]}}}";

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(0, errors.size());
    }


    @Test
    void testApplyValidation_throwsException() throws Exception {
        String json = "{\"name\": \"John Doe\", \"age\": 30, \"email\": \"john.doe@example.com\", \"password\": \"password123\", \"confirmPassword\": \"password123\", \"roles\": [\"admin\", \"user\"], \"settings\": {\"theme\": \"dark\", \"notifications\": true}}";
        String schema = "{\"properties\": {\"name\": {\"type\": \"string\", \"minSize\": 3, \"maxSize\": 50, \"required\": true}, \"age\": {\"type\": \"number\", \"minValue\": 18, \"maxValue\": 100, \"required\": true}, \"email\": {\"type\": \"string\", \"pattern\": \"^[^@\\\\s]+@[^@\\\\s]+\\\\.[^@\\\\s]+$\", \"required\": true}, \"password\": {\"type\": \"string\", \"minSize\": 8, \"required\": true}, \"confirmPassword\": {\"type\": \"string\", \"compare\": {\"compareTo\":\"carrierDetails.etd\",\"operator\":\"equals\"}, \"required\": true}, \"roles\": {\"type\": \"array\", \"minSize\": 1, \"enum\": [\"admin\", \"user\", \"guest\"], \"arrayProperties\": {\"type\": \"string\"}, \"required\": true}, \"settings\": {\"type\": \"object\", \"properties\": {\"theme\": {\"type\": \"string\", \"enum\": [\"light\", \"dark\"], \"required\": true}, \"notifications\": {\"type\": \"boolean\", \"required\": true}}, \"required\": true}}}";

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        assertThrows(ClassCastException.class, () -> validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false));

    }

}