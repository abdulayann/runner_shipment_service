package com.dpw.runner.shipment.services.dao.impl;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.PackingRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.PackingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.PickupDeliveryDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IPickupDeliveryDetailsRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.io.IOException;
import java.util.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class PickupDeliveryDetailsDaoTest {
    @Mock
    private IPickupDeliveryDetailsRepository pickupDeliveryDetailsRepository;

    @InjectMocks
    private PickupDeliveryDetailsDao pickupDeliveryDetailsDao;

    private static JsonTestUtility jsonTestUtility;

    private static ObjectMapper objectMapperTest;
    private PickupDeliveryDetails testPickupDeliveryDetails;

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
        testPickupDeliveryDetails = jsonTestUtility.getTestPickupDeliveryDetails();
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
    void testSave_Success() {
        PickupDeliveryDetails pickupDeliveryDetails = testPickupDeliveryDetails;
        when(pickupDeliveryDetailsRepository.save(pickupDeliveryDetails)).thenReturn(pickupDeliveryDetails);
        PickupDeliveryDetails response = pickupDeliveryDetailsDao.save(pickupDeliveryDetails);
        assertEquals(pickupDeliveryDetails, response);
    }

    @Test
    void testFindAll_Success() {
        Page<PickupDeliveryDetails> pickupDeliveryPage = mock(Page.class);
        Specification<PickupDeliveryDetails> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        when(pickupDeliveryDetailsRepository.findAll(spec, pageable)).thenReturn(pickupDeliveryPage);
        Page<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findAll(spec, pageable);
        assertEquals(pickupDeliveryPage, pickupDeliveryDetails);
    }

    @Test
    void testFindById_Success() {
        Optional<PickupDeliveryDetails> optionalPickupDeliveryDetails = Optional.of(testPickupDeliveryDetails);
        when(pickupDeliveryDetailsRepository.findById(anyLong())).thenReturn(optionalPickupDeliveryDetails);
        Optional<PickupDeliveryDetails> pickupDeliveryDetails = pickupDeliveryDetailsDao.findById(1L);
        assertTrue(pickupDeliveryDetails.isPresent());
        assertEquals(testPickupDeliveryDetails, pickupDeliveryDetails.get());
    }

    @Test
    void testDelete_Success() {
        PickupDeliveryDetails pickupDeliveryDetails = testPickupDeliveryDetails;

        assertDoesNotThrow(() -> pickupDeliveryDetailsDao.delete(pickupDeliveryDetails));
        verify(pickupDeliveryDetailsRepository, Mockito.times(1)).delete(pickupDeliveryDetails);
    }

    @Test
    void testUpdateEntityFromShipment_Success() throws RunnerException {
        List<PickupDeliveryDetails> pickupDeliveryDetailsList = Collections.singletonList(testPickupDeliveryDetails);

        var spyService = Mockito.spy(pickupDeliveryDetailsDao);
        doReturn(pickupDeliveryDetailsList).when(spyService).saveEntityFromShipment(anyList(), anyLong());

        List<PickupDeliveryDetails> pickupDeliveryDetails = spyService.updateEntityFromShipment(pickupDeliveryDetailsList, 1L);
        assertNotNull(pickupDeliveryDetails);
        assertEquals(pickupDeliveryDetailsList, pickupDeliveryDetails);
    }

    @Test
    void testUpdateEntityFromShipment_Failure1() throws RunnerException {
        List<PickupDeliveryDetails> pickupDeliveryDetailsList = Collections.singletonList(testPickupDeliveryDetails);
        var spyService = Mockito.spy(pickupDeliveryDetailsDao);
        assertThrows(RunnerException.class, ()-> spyService.updateEntityFromShipment(pickupDeliveryDetailsList, 1L));
    }

    @Test
    void testUpdateEntityFromShipment_Failure2() throws RunnerException {
        List<PickupDeliveryDetails> pickupDeliveryDetailsList = Collections.singletonList(testPickupDeliveryDetails);
        List<PickupDeliveryDetails> pickupDeliveryDetailsListOld = new ArrayList<>(Arrays.asList(testPickupDeliveryDetails));
        PickupDeliveryDetails pickupDeliveryDetails = jsonTestUtility.getTestPickupDeliveryDetails();
        pickupDeliveryDetails.setId(2L);
        pickupDeliveryDetailsListOld.add(pickupDeliveryDetails);
        var spyService = Mockito.spy(pickupDeliveryDetailsDao);
        doReturn(pickupDeliveryDetailsList).when(spyService).saveEntityFromShipment(anyList(), anyLong());

        List<PickupDeliveryDetails> pickupDeliveryDetailsResponse = spyService.updateEntityFromShipment(pickupDeliveryDetailsList, 1L);
        assertNotNull(pickupDeliveryDetailsResponse);
        assertEquals(pickupDeliveryDetailsList, pickupDeliveryDetailsResponse);
    }

    @Test
    void testSaveEntityFromShipment_Success() {
        List<PickupDeliveryDetails> pickupDeliveryDetailsList = Collections.singletonList(testPickupDeliveryDetails);
        var spyService = Mockito.spy(pickupDeliveryDetailsDao);

        doReturn(Optional.of(testPickupDeliveryDetails)).when(spyService).findById(anyLong());
        doReturn(testPickupDeliveryDetails).when(spyService).save(any());

        List<PickupDeliveryDetails> responseEntity = spyService.saveEntityFromShipment(pickupDeliveryDetailsList, 1L);
        assertNotNull(responseEntity);
        assertEquals(pickupDeliveryDetailsList, responseEntity);
    }

    @Test
    void testSaveEntityFromShipment_Failure() {
        List<PickupDeliveryDetails> pickupDeliveryDetailsList = Collections.singletonList(testPickupDeliveryDetails);
        var spyService = Mockito.spy(pickupDeliveryDetailsDao);
        doReturn(Optional.empty()).when(spyService).findById(anyLong());
        assertThrows(DataRetrievalFailureException.class, () -> spyService.saveEntityFromShipment(pickupDeliveryDetailsList, 1L));
    }

}
