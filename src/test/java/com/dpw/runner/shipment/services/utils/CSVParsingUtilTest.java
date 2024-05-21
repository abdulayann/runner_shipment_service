package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.ConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.response.ContainerResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;

class CSVParsingUtilTest {

    @Mock
    private ConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    private IShipmentDao shipmentDao;

    @Mock
    private IV1Service v1Service;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @InjectMocks
    private CSVParsingUtil<?> csvParsingUtil;

    private ContainerResponse containerResponse;

    private JsonTestUtility jsonTestUtility;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() throws IOException {
        objectMapper = JsonTestUtility.getMapper();
        jsonTestUtility = new JsonTestUtility();
        containerResponse = objectMapper.convertValue(jsonTestUtility.getTestContainer(), ContainerResponse.class);
        csvParsingUtil = new CSVParsingUtil<Containers>(consoleShipmentMappingDao, shipmentDao, v1Service, jsonHelper, consolidationDetailsDao);
        csvParsingUtil.executorService = Executors.newFixedThreadPool(10);
    }

    final Set<String> requiredFields = Set.of(Constants.CONTAINER_NUMBER, "volumeUtilization", "weightUtilization", "achievedVolume",
            "achievedVolumeUnit", "achievedWeight", "achievedWeightUnit", "grossVolume", "grossVolumeUnit",
            "allocatedWeight", "allocatedWeightUnit", Constants.NET_WEIGHT, "netWeightUnit", Constants.GROSS_WEIGHT, "grossWeightUnit", "remarks",
            "extraParams", "chargeable", "chargeableUnit", "ownType", Constants.PACKS, "packsType", "marksNums", "innerPackageMeasurementUnit", "pacrNumber");

    private final Set<String> hiddenFields = Set.of("pickupAddress",
            "deliveryAddress", "eventsList", "packsList", "shipmentsList", "bookingCharges");

    @Test
    void getHeadersForContainer() {
        List<Field> containerFields = Arrays.stream(ContainerResponse.class.getDeclaredFields()).toList();
        var response = csvParsingUtil.getHeadersForContainer();
        final Set<String> requiredFields = Set.of(Constants.CONTAINER_NUMBER, "volumeUtilization", "weightUtilization", "achievedVolume",
                "achievedVolumeUnit", "achievedWeight", "achievedWeightUnit", "grossVolume", "grossVolumeUnit",
                "allocatedWeight", "allocatedWeightUnit", Constants.NET_WEIGHT, "netWeightUnit", Constants.GROSS_WEIGHT, "grossWeightUnit", "remarks",
                "extraParams", "chargeable", "chargeableUnit", "ownType", Constants.PACKS, "packsType", "marksNums", "innerPackageMeasurementUnit", "pacrNumber");
        var expected = containerFields.stream().filter(field -> {
            if (requiredFields.contains(field.getName())) return true;
            else return false;
        }).map(x -> x.getName()).toList();
        assertEquals(response, expected);
    }

    @Test
    void testGetAllAttributeValuesAsListContainer() throws IllegalAccessException {
        ContainerResponse response = containerResponse;
        List<String> actualValues = csvParsingUtil.getAllAttributeValuesAsListContainer(response);
        assertNotNull(actualValues);
        assertNotNull(response);
    }

    @Test
    void testGetAllAttributeValuesAsListContainer_withNullValues() throws IllegalAccessException {
        ContainerResponse response = new ContainerResponse();

        List<String> expectedValues = Arrays.asList(
                "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
        );

        List<String> actualValues = csvParsingUtil.getAllAttributeValuesAsListContainer(response);

        assertEquals(expectedValues.size(), actualValues.size());
        assertEquals(expectedValues, actualValues);
    }
}