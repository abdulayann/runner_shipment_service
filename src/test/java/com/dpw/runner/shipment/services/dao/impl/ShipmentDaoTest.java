package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.repository.interfaces.IShipmentRepository;
import com.dpw.runner.shipment.services.validator.ValidatorUtility;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.dao.DataRetrievalFailureException;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class ShipmentDaoTest {

    @InjectMocks
    private ShipmentDao shipmentDao;

    @Mock
    private IShipmentRepository shipmentRepository;
    @Mock
    private ValidatorUtility validatorUtility;
    @Mock
    private JsonHelper jsonHelper;

    @Test
    void saveTestCatch() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(Arrays.asList(Containers.builder().build()));
        shipmentDetails.setConsolidationList(Arrays.asList(ConsolidationDetails.builder().build()));
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        assertThrows(RunnerException.class, () -> {
            shipmentDao.save(shipmentDetails, false);
        });
    }

    @Test
    void saveTestOldEntityNotPresent() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(Arrays.asList(Containers.builder().build()));
        shipmentDetails.setConsolidationList(Arrays.asList(ConsolidationDetails.builder().build()));
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.empty());
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        assertThrows(DataRetrievalFailureException.class, () -> {
            shipmentDao.save(shipmentDetails, false);
        });
    }

    @Test
    void saveTest() throws RunnerException {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());
        ShipmentDetails shipmentDetails = ShipmentDetails.builder().build();
        shipmentDetails.setContainersList(Arrays.asList(Containers.builder().build()));
        shipmentDetails.setConsolidationList(Arrays.asList(ConsolidationDetails.builder().build()));
        shipmentDetails.setCarrierDetails(CarrierDetails.builder().origin("origin").destination("destination").originPort("originPort").destinationPort("destinationPort").build());
        shipmentDetails.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        shipmentDetails.setId(1L);

        when(shipmentRepository.findById(any())).thenReturn(Optional.of(shipmentDetails));
        when(shipmentRepository.save(any(ShipmentDetails.class))).thenReturn(shipmentDetails);
        when(validatorUtility.applyValidation(any(), any(), any(), anyBoolean())).thenReturn(new HashSet<>());

        assertEquals(shipmentDetails, shipmentDao.save(shipmentDetails, false));
    }
}
