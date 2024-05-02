package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IHblTermsConditionTemplateDao;
import com.dpw.runner.shipment.services.dao.interfaces.IProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITenantProductsDao;
import com.dpw.runner.shipment.services.dto.request.ShipmentSettingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ShipmentSettingsDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class ShipmentSettingsServiceTest {

    @Mock
    IShipmentSettingsDao shipmentSettingsDao;

    @Mock
    IHblTermsConditionTemplateDao hblTermsConditionTemplateDao;

    @Mock
    ITenantProductsDao tenantProductsDao;

    @Mock
    IProductSequenceConfigDao productSequenceConfigDao;

    @Mock
    IShipmentSettingsSync shipmentSettingsSync;

    @Mock
    JsonHelper jsonHelper;

    @InjectMocks
    private ShipmentSettingsService shipmentSettingsService;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    private static ShipmentSettingsDetails testShipmentSettingsDetails;
    private static ShipmentSettingsDetails testShipmentSettingsDetails_New;
    private static ShipmentSettingRequest shipmentSettingRequest;
    private static ShipmentSettingsDetailsResponse shipmentSettingsDetailsResponse;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testShipmentSettingsDetails = jsonTestUtility.getTestShipmentSettingsDetails();
        testShipmentSettingsDetails_New = jsonTestUtility.getTestShipmentSettingsDetails_CreatePayload();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void create() {
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails_New, ShipmentSettingRequest.class);
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        doReturn(objectMapperTest.convertValue(testShipmentSettingsDetails_New, ShipmentSettingsDetails.class)).when(spyService).convertRequestToEntity(any());
        when(shipmentSettingsDao.save(any())).thenReturn(testShipmentSettingsDetails);
        when(hblTermsConditionTemplateDao.saveEntityFromSettings(any(), anyLong(), eq(true))).thenReturn(testShipmentSettingsDetails.getHblTermsConditionTemplate());
        when(hblTermsConditionTemplateDao.saveEntityFromSettings(any(), anyLong(), eq(false))).thenReturn(testShipmentSettingsDetails.getHblHawbBackPrintTemplate());
        when(tenantProductsDao.saveEntityFromSettings(any(), anyLong())).thenReturn(testShipmentSettingsDetails.getTenantProducts());
        when(tenantProductsDao.findAll(any(), any())).thenReturn(new PageImpl<>(testShipmentSettingsDetails.getTenantProducts()));
        when(productSequenceConfigDao.saveEntityFromSettings(any(), anyLong())).thenReturn(testShipmentSettingsDetails.getProductSequenceConfig());
        when(shipmentSettingsSync.sync(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetailsResponse.class))).thenReturn(objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingsDetailsResponse.class));
        ResponseEntity<IRunnerResponse> responseEntity = spyService.create(CommonRequestModel.buildRequest(shipmentSettingRequest));
        Assertions.assertNotNull(responseEntity);
        Assertions.assertEquals(responseEntity.getStatusCode(), HttpStatus.OK);
    }

    @Test
    void create_Failure() {
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        when(shipmentSettingsDao.save(any())).thenThrow(new RuntimeException());
        var e = Assertions.assertThrows(RuntimeException.class, () -> spyService.create(CommonRequestModel.buildRequest()));
        Assertions.assertNotNull(e);
    }

    @Test
    void update() {
        shipmentSettingsService.update(CommonRequestModel.buildRequest()); // test case not required since function is empty
    }

    @Test
    void completeUpdate() throws RunnerException {
        shipmentSettingRequest = objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingRequest.class);
        ShipmentSettingsService spyService = spy(shipmentSettingsService);
        when(shipmentSettingsDao.list(any(), any())).thenReturn(new PageImpl<>(List.of(testShipmentSettingsDetails)));
        doReturn(objectMapperTest.convertValue(testShipmentSettingsDetails_New, ShipmentSettingsDetails.class)).when(spyService).convertRequestToEntity(any());
        when(shipmentSettingsDao.save(any())).thenReturn(testShipmentSettingsDetails);
        when(hblTermsConditionTemplateDao.updateEntityFromSettings(any(), anyLong(), eq(true))).thenReturn(testShipmentSettingsDetails.getHblTermsConditionTemplate());
        when(hblTermsConditionTemplateDao.updateEntityFromSettings(any(), anyLong(), eq(false))).thenReturn(testShipmentSettingsDetails.getHblHawbBackPrintTemplate());
        when(tenantProductsDao.updateEntityFromSettings(any(), anyLong())).thenReturn(testShipmentSettingsDetails.getTenantProducts());
        when(tenantProductsDao.findAll(any(), any())).thenReturn(new PageImpl<>(testShipmentSettingsDetails.getTenantProducts()));
        when(productSequenceConfigDao.updateEntityFromSettings(any(), anyLong())).thenReturn(testShipmentSettingsDetails.getProductSequenceConfig());
        when(shipmentSettingsSync.sync(any())).thenThrow(new RuntimeException());
        when(jsonHelper.convertValue(any(), eq(ShipmentSettingsDetailsResponse.class))).thenReturn(objectMapperTest.convertValue(testShipmentSettingsDetails, ShipmentSettingsDetailsResponse.class));
        ResponseEntity<IRunnerResponse> responseEntity = spyService.completeUpdate(CommonRequestModel.buildRequest(shipmentSettingRequest));
        Assertions.assertEquals(responseEntity.getStatusCode(), HttpStatus.OK);
    }

}
