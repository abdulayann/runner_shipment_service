package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.BulkDownloadRequest;
import com.dpw.runner.shipment.services.dao.impl.ShipmentsContainersMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.ContainersExcelModel;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.kafka.producer.KafkaProducer;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@Execution(CONCURRENT)
class ContainerV3UtilTest extends CommonMocks {

    private static Containers testContainer;

    private static JsonTestUtility jsonTestUtility;

    @Mock
    private ShipmentsContainersMappingDao shipmentsContainersMappingDao;

    @Mock
    private IV1Service v1Service;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private ContainerValidationUtil containerValidationUtil;
    @Mock
    private IShipmentDao shipmentDao;
    @Mock
    private KafkaProducer producer;

    @Mock
    private IContainerDao containerDao;

    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @InjectMocks
    private ContainerV3Util containerV3Util;

    private static ObjectMapper objectMapper;

    private static ShipmentDetails testShipment;


    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            testContainer = jsonTestUtility.getTestContainer();
            objectMapper = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testContainer = jsonTestUtility.getTestContainer();
        testShipment = jsonTestUtility.getTestShipment();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").multipleShipmentEnabled(true).build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void downloadContainers(){
        HttpServletResponse response = new MockHttpServletResponse();
        BulkDownloadRequest request = new BulkDownloadRequest();
        request.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        request.setConsolidationId("3");
        request.setShipmentId("6");
        when(shipmentDao.findById(any())).thenReturn(Optional.of(testShipment));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(jsonTestUtility.getTestConsolidation()));
        when(containerDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(testContainer)));
        when(commonUtils.convertToList(anyList(), eq(ContainersExcelModel.class))).thenReturn(List.of(objectMapper.convertValue(testContainer, ContainersExcelModel.class)));
        assertDoesNotThrow(() -> containerV3Util.downloadContainers(response, request));
    }
}
