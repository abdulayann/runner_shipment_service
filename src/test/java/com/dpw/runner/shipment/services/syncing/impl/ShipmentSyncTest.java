package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.syncing.interfaces.IConsolidationSync;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.HttpStatus;

import java.time.LocalDateTime;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentSyncTest {

    @InjectMocks
    private ShipmentSync shipmentSync;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;
    @Mock
    private IConsolidationDetailsDao consolidationDetailsDao;
    @Mock
    private V1AuthHelper v1AuthHelper;
    @Mock
    private SyncEntityConversionService syncEntityConversionService;
    @Mock
    private ISyncService syncService;
    @Mock
    private IConsolidationSync consolidationSync;

    /**
     * Method under test:
     * {@link ShipmentSync#sync(ShipmentDetails, List, List, String, boolean)}
     */
    @Test
    void testSync() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputAddressMapWithOutRawData = new HashMap<String, Object>();

        ShipmentDetails inputShipment = new ShipmentDetails();

        AdditionalDetails inputAdditionalDetails = new AdditionalDetails();
        inputAdditionalDetails.setAndesStatus(AndesStatus.SendToAndes);
        inputAdditionalDetails.setOwnership(Ownership.Self);
        inputAdditionalDetails.setPassedBy(Ownership.Self);
        inputAdditionalDetails.setBOEDate(LocalDateTime.now());
        inputAdditionalDetails.setIGMFileNo(StringUtility.getRandomString(10));

        TruckDriverDetails inputTruckDriverDetails1 = new TruckDriverDetails();
        inputTruckDriverDetails1.setTransporterType(Ownership.Self);

        Containers inputContainer1 = new Containers();
        inputContainer1.setId(22L);
        inputContainer1.setGuid(UUID.randomUUID());

        Containers inputContainer2 = new Containers();
        inputContainer2.setId(33L);
        inputContainer2.setGuid(UUID.randomUUID());

        inputShipment.setId(1L);
        inputShipment.setGuid(guid);
        inputShipment.setAdditionalDetails(inputAdditionalDetails);
        inputShipment.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        inputShipment.setServicesList(List.of(new ServiceDetails()));
        inputShipment.setTruckDriverDetails(List.of(inputTruckDriverDetails1));
        inputShipment.setContainersList(Set.of(inputContainer1, inputContainer2));
        inputShipment.setConsigner(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        inputShipment.setConsignee(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        inputAdditionalDetails.setNotifyParty(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        inputShipment.setEventsList(List.of(new Events()));
        // Mock
        var mockCustomShipmentSyncRequest = new CustomShipmentSyncRequest();
        mockCustomShipmentSyncRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        when(modelMapper.map(any(), eq(CustomShipmentSyncRequest.class))).thenReturn(mockCustomShipmentSyncRequest);
        doNothing().when(modelMapper).map(inputShipment.getAdditionalDetails(), mockCustomShipmentSyncRequest);

        when(modelMapper.map(any(), eq(ShipmentServiceRequestV2.class))).thenReturn(new ShipmentServiceRequestV2());
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(new ConsolidationDetails());
        when(modelMapper.map(any(), eq(TruckDriverDetailsRequestV2.class))).thenReturn(new TruckDriverDetailsRequestV2());
        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(new PartyRequestV2());
        when(modelMapper.map(any(), eq(EventsRequestV2.class))).thenReturn(new EventsRequestV2());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));

        var responseEntity = shipmentSync.sync(inputShipment, List.of(), List.of(), StringUtility.convertToString(guid), true);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testSync2() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputAddressMapWithOutRawData = new HashMap<String, Object>();
        inputAddressMapWithOutRawData.put(PartiesConstants.RAW_DATA, "Raw-data");
        ShipmentDetails inputShipment = new ShipmentDetails();

        AdditionalDetails inputAdditionalDetails = new AdditionalDetails();
        inputAdditionalDetails.setAndesStatus(AndesStatus.SendToAndes);
        inputAdditionalDetails.setOwnership(Ownership.ThirdParty);
        inputAdditionalDetails.setBOEDate(LocalDateTime.now());
        inputAdditionalDetails.setIGMFileNo(StringUtility.getRandomString(10));

        TruckDriverDetails inputTruckDriverDetails1 = new TruckDriverDetails();
        inputTruckDriverDetails1.setTransporterType(Ownership.Self);


        Containers inputContainer1 = new Containers();
        inputContainer1.setId(22L);
        inputContainer1.setGuid(UUID.randomUUID());

        Containers inputContainer2 = new Containers();
        inputContainer2.setId(33L);
        inputContainer2.setGuid(UUID.randomUUID());

        inputShipment.setId(1L);
        inputShipment.setGuid(guid);
        inputShipment.setAdditionalDetails(inputAdditionalDetails);
        inputShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        inputShipment.setContainersList(Set.of(inputContainer1, inputContainer2));
        inputShipment.setConsigner(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        inputShipment.setConsignee(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        inputAdditionalDetails.setNotifyParty(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        // Mock
        var mockCustomShipmentSyncRequest = new CustomShipmentSyncRequest();
        mockCustomShipmentSyncRequest.setTransportMode(Constants.TRANSPORT_MODE_AIR);

        when(modelMapper.map(any(), eq(CustomShipmentSyncRequest.class))).thenReturn(mockCustomShipmentSyncRequest);
        doNothing().when(modelMapper).map(inputShipment.getAdditionalDetails(), mockCustomShipmentSyncRequest);
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(new ConsolidationDetails());
        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(new PartyRequestV2());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));

        var responseEntity = shipmentSync.sync(inputShipment, List.of(), List.of(), StringUtility.convertToString(guid), true);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void testSync3() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputAddressMapWithOutRawData = new HashMap<String, Object>();
        inputAddressMapWithOutRawData.put(PartiesConstants.RAW_DATA, "Raw-data");
        ShipmentDetails inputShipment = new ShipmentDetails();

        AdditionalDetails inputAdditionalDetails = new AdditionalDetails();
        inputAdditionalDetails.setBOEDate(LocalDateTime.now());
        inputAdditionalDetails.setIGMFileNo(StringUtility.getRandomString(10));

        TruckDriverDetails inputTruckDriverDetails1 = new TruckDriverDetails();
        inputTruckDriverDetails1.setTransporterType(Ownership.Self);


        inputShipment.setId(1L);
        inputShipment.setGuid(guid);
        inputShipment.setAdditionalDetails(inputAdditionalDetails);
        inputShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        inputShipment.setContainersList(Set.of());
        inputShipment.setTruckDriverDetails(List.of(inputTruckDriverDetails1));
        inputShipment.setConsigner(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(false).build());
        inputShipment.setConsignee(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(false).build());
        inputAdditionalDetails.setNotifyParty(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(false).build());
        // Mock
        var mockCustomShipmentSyncRequest = new CustomShipmentSyncRequest();
        mockCustomShipmentSyncRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        when(modelMapper.map(any(), eq(CustomShipmentSyncRequest.class))).thenReturn(mockCustomShipmentSyncRequest);
        doNothing().when(modelMapper).map(inputShipment.getAdditionalDetails(), mockCustomShipmentSyncRequest);
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(new ConsolidationDetails());
        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(new PartyRequestV2());
        when(modelMapper.map(any(), eq(TruckDriverDetailsRequestV2.class))).thenReturn(new TruckDriverDetailsRequestV2());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));

        var responseEntity = shipmentSync.sync(inputShipment, List.of(), List.of(), StringUtility.convertToString(guid), false);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void syncLockStatus() {
        boolean isSuccess = true;
        var input = new ShipmentDetails();
        input.setIsLocked(true);
        input.setGuid(UUID.randomUUID());
        shipmentSync.syncLockStatus(input);
        assertTrue(isSuccess);
    }


    @Test
    void syncFromBooking() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputAddressMapWithOutRawData = new HashMap<String, Object>();
        inputAddressMapWithOutRawData.put(PartiesConstants.RAW_DATA, "Raw-data");
        ShipmentDetails inputShipment = new ShipmentDetails();

        AdditionalDetails inputAdditionalDetails = new AdditionalDetails();
        inputAdditionalDetails.setBOEDate(LocalDateTime.now());
        inputAdditionalDetails.setIGMFileNo(StringUtility.getRandomString(10));

        TruckDriverDetails inputTruckDriverDetails1 = new TruckDriverDetails();
        inputTruckDriverDetails1.setTransporterType(Ownership.Self);


        inputShipment.setId(1L);
        inputShipment.setGuid(guid);
        inputShipment.setAdditionalDetails(inputAdditionalDetails);
        inputShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        inputShipment.setContainersList(Set.of());
        inputShipment.setTruckDriverDetails(List.of(inputTruckDriverDetails1));
        inputShipment.setConsigner(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(false).build());
        inputShipment.setConsignee(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(false).build());
        inputAdditionalDetails.setNotifyParty(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(false).build());
        // Mock
        var mockCustomShipmentSyncRequest = new CustomShipmentSyncRequest();
        mockCustomShipmentSyncRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        when(modelMapper.map(any(), eq(CustomShipmentSyncRequest.class))).thenReturn(mockCustomShipmentSyncRequest);
        doNothing().when(modelMapper).map(inputShipment.getAdditionalDetails(), mockCustomShipmentSyncRequest);
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(new ConsolidationDetails());
        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(new PartyRequestV2());
        when(modelMapper.map(any(), eq(TruckDriverDetailsRequestV2.class))).thenReturn(new TruckDriverDetailsRequestV2());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));

        var responseEntity = shipmentSync.syncFromBooking(inputShipment, List.of(), List.of());

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    }

    @Test
    void syncFromBooking2() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        ShipmentDetails inputShipment = new ShipmentDetails();

        inputShipment.setId(1L);
        inputShipment.setGuid(guid);
        inputShipment.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        inputShipment.setConsolidationList(Set.of(new ConsolidationDetails()));
        // Mock
        var mockCustomShipmentSyncRequest = new CustomShipmentSyncRequest();
        mockCustomShipmentSyncRequest.setTransportMode(Constants.TRANSPORT_MODE_SEA);

        when(modelMapper.map(any(), eq(CustomShipmentSyncRequest.class))).thenReturn(mockCustomShipmentSyncRequest);
        when(consoleShipmentMappingDao.findByShipmentId(any())).thenReturn(List.of(new ConsoleShipmentMapping()));
        when(consolidationDetailsDao.findConsolidationsById(any())).thenReturn(new ConsolidationDetails());
        when(jsonHelper.convertToJson(any())).thenReturn(StringUtility.getRandomString(100));
        when(consolidationDetailsDao.findById(any())).thenReturn(Optional.of(new ConsolidationDetails()));
        when(consolidationSync.createConsoleSyncReq(any())).thenReturn(new CustomConsolidationRequest());

        var responseEntity = shipmentSync.syncFromBooking(inputShipment, List.of(), List.of());

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());

    }


}
