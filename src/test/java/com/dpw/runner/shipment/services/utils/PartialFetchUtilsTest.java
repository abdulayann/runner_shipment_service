package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.github.bohnman.squiggly.Squiggly;
import com.github.bohnman.squiggly.util.SquigglyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class PartialFetchUtilsTest {

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private PartialFetchUtils partialFetchUtils;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testFetchPartialData_NullIncludeColumns() {
        RunnerResponse<String> response = new RunnerResponse<>();
        response.setData("testData");

        Object result = partialFetchUtils.fetchPartialData(response, null);

        assertEquals(response, result);
    }

    @Test
    void testFetchPartialData_EmptyIncludeColumns() {
        RunnerResponse<String> response = new RunnerResponse<>();
        response.setData("testData");

        Object result = partialFetchUtils.fetchPartialData(response, Collections.emptyList());

        assertEquals(response, result);
    }

    @Test
    void testFetchPartialData_WithIncludeColumns() throws Exception {
        RunnerResponse<String> response = new RunnerResponse<>();
        response.setData("testData");

        List<String> includeColumns = List.of("data");

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        ObjectMapper modified = Squiggly.init(objectMapper, String.join(",", includeColumns));
        String jsonString = SquigglyUtils.stringify(modified, response.getData());
//        when(jsonHelper.readFromJson(jsonString, Object.class)).thenReturn("partialData");

        Object result = partialFetchUtils.fetchPartialData(response, includeColumns);

        assertNull(result);
    }

    @Test
    void testFetchPartialListData_NullIncludeColumns() {
        IRunnerResponse response = mock(IRunnerResponse.class);

        Object result = partialFetchUtils.fetchPartialListData(response, null);

        assertEquals(response, result);
    }

    @Test
    void testFetchPartialListData_EmptyIncludeColumns() {
        IRunnerResponse response = mock(IRunnerResponse.class);

        Object result = partialFetchUtils.fetchPartialListData(response, Collections.emptyList());

        assertEquals(response, result);
    }

    @Test
    void testFetchPartialListData_WithIncludeColumns() throws Exception {
        IRunnerResponse response = mock(IRunnerResponse.class);

        List<String> includeColumns = List.of("data");

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        ObjectMapper modified = Squiggly.init(objectMapper, String.join(",", includeColumns));
        String jsonString = SquigglyUtils.stringify(modified, response);
//        when(jsonHelper.readFromJson(jsonString, Object.class)).thenReturn("partialData");

        Object result = partialFetchUtils.fetchPartialListData(response, includeColumns);

        assertNull(result);
    }

}
