package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertNotNull;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DbAccessHelperTest {

    @InjectMocks
    private DbAccessHelper dbAccessHelper;
    private static JsonTestUtility jsonTestUtility;
    private static ListCommonRequest listCommonRequest;
    private static ObjectMapper objectMapperTest;

    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapperTest = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());

        listCommonRequest = jsonTestUtility.getListRequest();
    }

    @Test
    void fetchDataTableNamesNull() {
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }

    @Test
    void fetchData() {
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class));
    }

    @Test
    void fetchDataTableNamesNotNullDoesNotContainText() {
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName("shipment_details").build());
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }

    @Test
    void fetchDataTableNamesNotNull() {
        listCommonRequest.setContainsText("transportMode");
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).build());
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }

    @Test
    void fetchDataTableNamesNotNullLogicalOperator() {
        listCommonRequest.setContainsText("transportMode");
        //FilterCriteria filterCriteria = listCommonRequest.getFilterCriteria().get(0);
//        FilterCriteria filterCriteria = FilterCriteria.builder().logicOperator("OR").build();
//        List<FilterCriteria> filterCriteriaList = Arrays.asList(filterCriteria);
//        listCommonRequest.setFilterCriteria(filterCriteriaList);
        //listCommonRequest.getFilterCriteria().set(0, FilterCriteria.builder().build());
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).build());
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }



}
