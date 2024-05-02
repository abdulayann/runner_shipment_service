package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IAdditionalDetailRepository;
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
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class AdditionalDetailDaoTest {

    @Mock
    private IAdditionalDetailRepository additionalDetailRepository;

    @InjectMocks
    private AdditionalDetailDao additionalDetailDao;

    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;
    private static AdditionalDetails testAdditionalDetails;

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
        testAdditionalDetails = jsonTestUtility.getTestAdditionalDetails();
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
    void save() {
        Mockito.when(additionalDetailRepository.save(testAdditionalDetails)).thenReturn(testAdditionalDetails);
        AdditionalDetails additionalDetails = additionalDetailDao.save(testAdditionalDetails);
        Assertions.assertNotNull(additionalDetails);
    }

    @Test
    void findById() {
        Mockito.when(additionalDetailRepository.findById(1L)).thenReturn(Optional.of(testAdditionalDetails));
        Optional<AdditionalDetails> additionalDetails = additionalDetailDao.findById(1L);
        Assertions.assertNotNull(additionalDetails);
    }

    @Test
    void updateEntityFromShipment() throws RunnerException {
        AdditionalDetailDao spyService = spy(additionalDetailDao);
        doReturn(Optional.of(testAdditionalDetails)).when(spyService).findById(any());
        doReturn(testAdditionalDetails).when(spyService).save(any());
        AdditionalDetails additionalDetails = spyService.updateEntityFromShipment(testAdditionalDetails);
        Assertions.assertNotNull(additionalDetails);
    }

    @Test
    void updateEntityFromShipment_Failure() throws RunnerException {
        AdditionalDetailDao spyService = spy(additionalDetailDao);
        doReturn(Optional.empty()).when(spyService).findById(any());
        var e = Assertions.assertThrows(RunnerException.class, () -> spyService.updateEntityFromShipment(testAdditionalDetails));
        Assertions.assertNotNull(e);
    }

}
