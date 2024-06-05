package com.dpw.runner.shipment.services.validator;

import com.dpw.runner.shipment.services.dao.interfaces.IValidationsDao;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ValidatorUtilityTest {

    final static String path = "src/test/java/com/dpw/runner/shipment/services/validator/";

    JsonTestUtility jsonTestUtility = new JsonTestUtility();
    @InjectMocks
    private ValidatorUtility validatorUtility;

    @Mock
    private IValidationsDao validationsDao;

    @Mock
    private ObjectMapper objectMapper;

    ValidatorUtilityTest() throws IOException {
    }


    @Test
    void testApplyValidation() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test1.json");
        String schema = jsonTestUtility.readJson(path + "Test1Schema.json");

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
        String json = jsonTestUtility.readJson(path + "Test2.json");
        String schema = jsonTestUtility.readJson(path + "Test2Schema.json");

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
        String json = jsonTestUtility.readJson(path + "Test2.json");
        String schema = jsonTestUtility.readJson(path + "Test3Schema.json");

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
        final String json = jsonTestUtility.readJson(path + "Test4.json");
        final String schema = jsonTestUtility.readJson(path + "Test4Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        assertThrows(ClassCastException.class, () -> validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false));

    }


    @Test
    void testApplyValidation3() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test5.json");
        String schema = jsonTestUtility.readJson(path + "Test5Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations testvalidation = new Validations();
        testvalidation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(testvalidation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(3, errors.size());
    }
}