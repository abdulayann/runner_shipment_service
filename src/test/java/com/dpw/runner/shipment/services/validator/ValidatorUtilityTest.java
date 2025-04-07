package com.dpw.runner.shipment.services.validator;

import com.dpw.runner.shipment.services.dao.interfaces.IValidationsDao;
import com.dpw.runner.shipment.services.entity.Validations;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
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


    @BeforeEach
    void setUp() {
        validatorUtility.clearValidationsMap();
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

    @Test
    void testApplyValidationSchemaNull() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test1.json");
        String schema = jsonTestUtility.readJson(path + "Test6Schema.json");

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
    void testApplyValidationFailOnFirst() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test1.json");
        String schema = jsonTestUtility.readJson(path + "Test1Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, true);

        assertEquals(1, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompare() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test7.json");
        String schema = jsonTestUtility.readJson(path + "Test7Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareTrue() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test8.json");
        String schema = jsonTestUtility.readJson(path + "Test7Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareFalse() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test7Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareDefault() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test10.json");
        String schema = jsonTestUtility.readJson(path + "Test7Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(7, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareNotEquals() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test7.json");
        String schema = jsonTestUtility.readJson(path + "Test8Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareNotEqualsString() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test4.json");
        String schema = jsonTestUtility.readJson(path + "Test8Schema.json");

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
    void testApplyValidationFailOnFirstValidateCompareNotEqualsTrue() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test8.json");
        String schema = jsonTestUtility.readJson(path + "Test8Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareNotEqualsFalse() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test8Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareNotEqualsDefault() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test8Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareLessarThan() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test7.json");
        String schema = jsonTestUtility.readJson(path + "Test9Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareLesserThanString() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test4.json");
        String schema = jsonTestUtility.readJson(path + "Test9Schema.json");

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
    void testApplyValidationFailOnFirstValidateCompareLesserThanTrue() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test8.json");
        String schema = jsonTestUtility.readJson(path + "Test9Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareLesserThanFalse() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test9Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareLesserThanDefault() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test9Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareLesserThanEqual() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test7.json");
        String schema = jsonTestUtility.readJson(path + "Test10Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareLesserThanEqualString() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test4.json");
        String schema = jsonTestUtility.readJson(path + "Test10Schema.json");

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
    void testApplyValidationFailOnFirstValidateCompareLesserThanEqualTrue() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test8.json");
        String schema = jsonTestUtility.readJson(path + "Test10Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareLesserThanEqualFalse() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test10Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareLesserThanEqualDefault() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test10Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareGreater() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test7.json");
        String schema = jsonTestUtility.readJson(path + "Test11Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareGreaterString() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test4.json");
        String schema = jsonTestUtility.readJson(path + "Test11Schema.json");

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
    void testApplyValidationFailOnFirstValidateCompareGreaterThanTrue() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test8.json");
        String schema = jsonTestUtility.readJson(path + "Test11Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareGreaterThanFalse() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test11Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareGreaterThanDefault() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test11Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareGreaterThanEqual() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test7.json");
        String schema = jsonTestUtility.readJson(path + "Test12Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(6, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareGreaterThanEqualString() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test4.json");
        String schema = jsonTestUtility.readJson(path + "Test12Schema.json");

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
    void testApplyValidationFailOnFirstValidateCompareGreaterThanEqualTrue() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test8.json");
        String schema = jsonTestUtility.readJson(path + "Test12Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareGreaterThanEqualFalse() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test12Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidationFailOnFirstValidateCompareGreaterThanEqaulDefault() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test9.json");
        String schema = jsonTestUtility.readJson(path + "Test12Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }

    @Test
    void testApplyValidation_testValidateCompareDateTime() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test11.json");
        String schema = jsonTestUtility.readJson(path + "Test2Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(2, errors.size());
    }

    @Test
    void testApplyValidation3Number() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test14.json");
        String schema = jsonTestUtility.readJson(path + "Test14Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations testvalidation = new Validations();
        testvalidation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(testvalidation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(0, errors.size());
    }

    @Test
    void testApplyValidation3NumberTrue() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test15.json");
        String schema = jsonTestUtility.readJson(path + "Test15Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations testvalidation = new Validations();
        testvalidation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(testvalidation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(1, errors.size());
    }

    @Test
    void testApplyValidation3NumberNotEquals() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test14.json");
        String schema = jsonTestUtility.readJson(path + "Test16Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations testvalidation = new Validations();
        testvalidation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(testvalidation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(0, errors.size());
    }

    @Test
    void testApplyValidation3NumberTrueIn() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test15.json");
        String schema = jsonTestUtility.readJson(path + "Test17Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations testvalidation = new Validations();
        testvalidation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(testvalidation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(0, errors.size());
    }

    @Test
    void testApplyValidationErrorNotNull() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test1.json");
        String schema = jsonTestUtility.readJson(path + "Test18Schema.json");

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
    void testApplyValidationErrorNotNullPattern() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test1.json");
        String schema = jsonTestUtility.readJson(path + "Test19Schema.json");

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
    void testApplyValidationErrorNotNullEmailPatternNotMatch() throws Exception {
        String json = jsonTestUtility.readJson(path + "Test19.json");
        String schema = jsonTestUtility.readJson(path + "Test19Schema.json");

        JsonReader schemaReader = Json.createReader(new StringReader(schema));
        JsonObject schemaObject = schemaReader.readObject();

        Validations validation = new Validations();
        validation.setJsonSchema(Map.of("",schemaObject));
        List<Validations> validationsList = Collections.singletonList(validation);
        when(validationsDao.findByLifecycleHookAndEntity(any(), any())).thenReturn(Optional.of(validationsList));
        when(objectMapper.writeValueAsString(any())).thenReturn(schema);
        Set<String> errors = validatorUtility.applyValidation(json, "entity", LifecycleHooks.ON_CREATE, false);

        assertEquals(5, errors.size());
    }
}