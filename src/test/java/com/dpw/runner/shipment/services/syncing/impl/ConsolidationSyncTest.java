package com.dpw.runner.shipment.services.syncing.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.aspects.sync.SyncingContext;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.ITruckDriverDetailsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.ArrivalDepartureDetails;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.ISyncService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.syncing.Entity.*;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.EmailServiceUtility;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ConsolidationSyncTest {

    @InjectMocks
    private ConsolidationSync consolidationSync;

    @Mock
    ModelMapper modelMapper;

    @Mock
    RestTemplate restTemplate;

    @Mock
    JsonHelper jsonHelper;

    @Mock
    IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Mock
    IShipmentDao shipmentDao;

    @Mock
    private IV1Service v1Service;
    @Mock
    private SyncEntityConversionService syncEntityConversionService;

    @Mock
    private EmailServiceUtility emailServiceUtility;
    @Mock
    private CommonUtils commonUtils;
    @Mock
    private ISyncService syncService;
    @Mock
    private ITruckDriverDetailsDao truckDriverDetailsDao;
    @Mock
    private V1AuthHelper v1AuthHelper;

    @BeforeEach
    void setUp() {
        SyncingContext.setContext(Boolean.TRUE);
    }

    /**
     * Method under test:
     * {@link ConsolidationSync#sync(ConsolidationDetails, String, boolean)}
     */
    @Test
    void testSync() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputConsolidation = new ConsolidationDetails();
        var inputAddressMapWithRawData = new HashMap<String, Object>();
        inputAddressMapWithRawData.put(PartiesConstants.RAW_DATA, "party name");
        var inputContainer1 = new Containers();
        inputContainer1.setId(11L);
        inputContainer1.setGuid(UUID.randomUUID());

        var inputContainer2 = new Containers();
        inputContainer2.setId(12L);
        inputContainer2.setGuid(UUID.randomUUID());

        var inputJobs = new Jobs();
        inputJobs.setEventsList(List.of());

        inputConsolidation.setGuid(guid);
        inputConsolidation.setCarrierDetails(CarrierDetails.builder().build());
        inputConsolidation.setAchievedQuantities(AchievedQuantities.builder().build());
        inputConsolidation.setAllocations(Allocations.builder().build());
        inputConsolidation.setContainersList(List.of(inputContainer1, inputContainer2));
        inputConsolidation.setArrivalDetails(
                ArrivalDepartureDetails.builder()
                        .containerYardId(Parties.builder().build())
                        .firstForeignPortId(Parties.builder().build())
                        .lastForeignPortId(Parties.builder().build())
                        .transportPortId(Parties.builder().build())
                        .CFSId(Parties.builder().build())
                        .CTOId(Parties.builder().build())
                        .build());
        inputConsolidation.setDepartureDetails(
                ArrivalDepartureDetails.builder()
                        .containerYardId(Parties.builder().build())
                        .firstForeignPortId(Parties.builder().build())
                        .lastForeignPortId(Parties.builder().build())
                        .transportPortId(Parties.builder().build())
                        .CFSId(Parties.builder().build())
                        .CTOId(Parties.builder().build())
                        .build()
        );
        inputConsolidation.setFileRepoList(List.of(new FileRepo()));
        inputConsolidation.setCreditor(Parties.builder().addressData(inputAddressMapWithRawData).isAddressFreeText(true).build());
        inputConsolidation.setReceivingAgent(Parties.builder().addressData(inputAddressMapWithRawData).isAddressFreeText(true).build());
        inputConsolidation.setSendingAgent(Parties.builder().addressData(inputAddressMapWithRawData).isAddressFreeText(true).build());
        inputConsolidation.setJobsList(List.of(inputJobs));
        inputConsolidation.setPackingList(List.of(new Packing()));
        var mockCustomConsolidationRequest = new CustomConsolidationRequest();
        var mockShipment = new ShipmentDetails();
        mockShipment.setId(11L);
        mockShipment.setGuid(UUID.randomUUID());
        mockShipment.setTenantId(1);

        var mockShipmentConsoleMapping = new ConsoleShipmentMapping();
        mockShipmentConsoleMapping.setConsolidationId(22L);
        mockShipmentConsoleMapping.setShipmentId(11L);

        var mockTruckDriverDetails1 = new TruckDriverDetails();
        mockTruckDriverDetails1.setShipmentId(11L);
        mockTruckDriverDetails1.setContainerId(11L);

        var mockTruckDriverDetails2 = new TruckDriverDetails();

        var mockTruckDriverDetailsRequestV2 = new TruckDriverDetailsRequestV2();

        when(modelMapper.map(any(), eq(CustomConsolidationRequest.class))).thenReturn(mockCustomConsolidationRequest);
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(mockShipmentConsoleMapping));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn((List.of(mockShipment)));
        when(truckDriverDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockTruckDriverDetails1, mockTruckDriverDetails2)));
        when(modelMapper.map(any(), eq(TruckDriverDetailsRequestV2.class))).thenReturn(mockTruckDriverDetailsRequestV2);
        doNothing().when(modelMapper).map(inputConsolidation.getCarrierDetails(), mockCustomConsolidationRequest);
        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(new PartyRequestV2());
        when(modelMapper.map(any(), eq(JobRequestV2.class))).thenReturn(new JobRequestV2());
        when(modelMapper.map(any(), eq(FileRepoRequestV2.class))).thenReturn(new FileRepoRequestV2());

        doNothing().when(modelMapper).map(List.of(), new ArrayList<>());


        var responseEntity = consolidationSync.sync(inputConsolidation, guid.toString(), true);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void testSync2() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputConsolidation = new ConsolidationDetails();
        var inputAddressMapWithRawData = new HashMap<String, Object>();
        var inputAddressMapWithoutRawData = new HashMap<String, Object>();
        inputAddressMapWithRawData.put(PartiesConstants.RAW_DATA, "party name");
        var inputContainer1 = new Containers();
        inputContainer1.setId(11L);
        inputContainer1.setGuid(UUID.randomUUID());

        var inputContainer2 = new Containers();
        inputContainer2.setId(12L);
        inputContainer2.setGuid(UUID.randomUUID());

        inputConsolidation.setGuid(guid);
        inputConsolidation.setContainersList(List.of(inputContainer1, inputContainer2));

        inputConsolidation.setCreditor(Parties.builder().addressData(inputAddressMapWithoutRawData).isAddressFreeText(false).build());
        inputConsolidation.setReceivingAgent(Parties.builder().addressData(inputAddressMapWithRawData).isAddressFreeText(false).build());
        inputConsolidation.setSendingAgent(Parties.builder().addressData(inputAddressMapWithRawData).isAddressFreeText(false).build());

        var mockCustomConsolidationRequest = new CustomConsolidationRequest();
        mockCustomConsolidationRequest.setAutoUpdateGoodsDesc(true);
        var mockShipment = new ShipmentDetails();
        mockShipment.setId(11L);
        mockShipment.setGuid(UUID.randomUUID());
        mockShipment.setTenantId(1);

        var mockShipmentConsoleMapping = new ConsoleShipmentMapping();
        mockShipmentConsoleMapping.setConsolidationId(22L);
        mockShipmentConsoleMapping.setShipmentId(11L);

        var mockTruckDriverDetails1 = new TruckDriverDetails();
        mockTruckDriverDetails1.setShipmentId(11L);
        mockTruckDriverDetails1.setContainerId(11L);

        var mockTruckDriverDetails2 = new TruckDriverDetails();

        var mockTruckDriverDetailsRequestV2 = new TruckDriverDetailsRequestV2();

        when(modelMapper.map(any(), eq(CustomConsolidationRequest.class))).thenReturn(mockCustomConsolidationRequest);
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(mockShipmentConsoleMapping));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn((List.of(mockShipment)));
        when(truckDriverDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockTruckDriverDetails1, mockTruckDriverDetails2)));
        when(modelMapper.map(any(), eq(TruckDriverDetailsRequestV2.class))).thenReturn(mockTruckDriverDetailsRequestV2);

        var responseEntity = consolidationSync.sync(inputConsolidation, guid.toString(), true);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testSync3() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputConsolidation = new ConsolidationDetails();
        var inputAddressMapWithRawData = new HashMap<String, Object>();
        var inputAddressMapWithoutRawData = new HashMap<String, Object>();
        inputAddressMapWithRawData.put(PartiesConstants.RAW_DATA, "party name");
        var inputContainer1 = new Containers();
        inputContainer1.setId(11L);
        inputContainer1.setGuid(UUID.randomUUID());

        var inputContainer2 = new Containers();
        inputContainer2.setId(12L);
        inputContainer2.setGuid(UUID.randomUUID());

        inputConsolidation.setGuid(guid);
        inputConsolidation.setContainersList(List.of(inputContainer1, inputContainer2));
        inputConsolidation.setArrivalDetails(ArrivalDepartureDetails.builder().build());
        inputConsolidation.setDepartureDetails(ArrivalDepartureDetails.builder().build());
        inputConsolidation.setCreditor(Parties.builder().addressData(inputAddressMapWithoutRawData).isAddressFreeText(false).build());
        inputConsolidation.setReceivingAgent(Parties.builder().addressData(inputAddressMapWithRawData).isAddressFreeText(false).build());
        inputConsolidation.setSendingAgent(Parties.builder().addressData(inputAddressMapWithRawData).isAddressFreeText(false).build());

        var mockCustomConsolidationRequest = new CustomConsolidationRequest();
        mockCustomConsolidationRequest.setAutoUpdateGoodsDesc(true);
        var mockShipment = new ShipmentDetails();
        mockShipment.setId(11L);
        mockShipment.setGuid(UUID.randomUUID());
        mockShipment.setTenantId(1);

        var mockShipmentConsoleMapping = new ConsoleShipmentMapping();
        mockShipmentConsoleMapping.setConsolidationId(22L);
        mockShipmentConsoleMapping.setShipmentId(11L);

        var mockTruckDriverDetails1 = new TruckDriverDetails();
        mockTruckDriverDetails1.setShipmentId(11L);
        mockTruckDriverDetails1.setContainerId(11L);

        var mockTruckDriverDetails2 = new TruckDriverDetails();

        var mockTruckDriverDetailsRequestV2 = new TruckDriverDetailsRequestV2();

        when(modelMapper.map(any(), eq(CustomConsolidationRequest.class))).thenReturn(mockCustomConsolidationRequest);
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(mockShipmentConsoleMapping));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn((List.of(mockShipment)));
        when(truckDriverDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockTruckDriverDetails1, mockTruckDriverDetails2)));
        when(modelMapper.map(any(), eq(TruckDriverDetailsRequestV2.class))).thenReturn(mockTruckDriverDetailsRequestV2);

        var responseEntity = consolidationSync.sync(inputConsolidation, guid.toString(), true);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void testSync4() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputConsolidation = new ConsolidationDetails();
        var inputAddressMapWithOutRawData = new HashMap<String, Object>();
        var inputContainer1 = new Containers();
        inputContainer1.setId(11L);
        inputContainer1.setGuid(UUID.randomUUID());

        var inputContainer2 = new Containers();
        inputContainer2.setId(12L);
        inputContainer2.setGuid(UUID.randomUUID());

        var inputJobs = new Jobs();
        inputJobs.setEventsList(List.of());

        inputConsolidation.setGuid(guid);
        inputConsolidation.setCarrierDetails(CarrierDetails.builder().build());
        inputConsolidation.setAchievedQuantities(AchievedQuantities.builder().build());
        inputConsolidation.setAllocations(Allocations.builder().build());
        inputConsolidation.setContainersList(List.of(inputContainer1, inputContainer2));
        inputConsolidation.setArrivalDetails(
                ArrivalDepartureDetails.builder()
                        .containerYardId(Parties.builder().build())
                        .firstForeignPortId(Parties.builder().build())
                        .lastForeignPortId(Parties.builder().build())
                        .transportPortId(Parties.builder().build())
                        .CFSId(Parties.builder().build())
                        .CTOId(Parties.builder().build())
                        .build());
        inputConsolidation.setDepartureDetails(
                ArrivalDepartureDetails.builder()
                        .containerYardId(Parties.builder().build())
                        .firstForeignPortId(Parties.builder().build())
                        .lastForeignPortId(Parties.builder().build())
                        .transportPortId(Parties.builder().build())
                        .CFSId(Parties.builder().build())
                        .CTOId(Parties.builder().build())
                        .build()
        );
        inputConsolidation.setFileRepoList(List.of(new FileRepo()));
        inputConsolidation.setCreditor(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        inputConsolidation.setReceivingAgent(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        inputConsolidation.setSendingAgent(Parties.builder().addressData(inputAddressMapWithOutRawData).isAddressFreeText(true).build());
        inputConsolidation.setJobsList(List.of(inputJobs));
        inputConsolidation.setPackingList(List.of(new Packing()));
        var mockCustomConsolidationRequest = new CustomConsolidationRequest();
        var mockShipment = new ShipmentDetails();
        mockShipment.setId(11L);
        mockShipment.setGuid(UUID.randomUUID());
        mockShipment.setTenantId(1);

        var mockShipmentConsoleMapping = new ConsoleShipmentMapping();
        mockShipmentConsoleMapping.setConsolidationId(22L);
        mockShipmentConsoleMapping.setShipmentId(11L);

        var mockTruckDriverDetails1 = new TruckDriverDetails();
        mockTruckDriverDetails1.setShipmentId(11L);
        mockTruckDriverDetails1.setContainerId(11L);

        var mockTruckDriverDetails2 = new TruckDriverDetails();

        var mockTruckDriverDetailsRequestV2 = new TruckDriverDetailsRequestV2();

        when(modelMapper.map(any(), eq(CustomConsolidationRequest.class))).thenReturn(mockCustomConsolidationRequest);
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(mockShipmentConsoleMapping));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn((List.of(mockShipment)));
        when(truckDriverDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockTruckDriverDetails1, mockTruckDriverDetails2)));
        when(modelMapper.map(any(), eq(TruckDriverDetailsRequestV2.class))).thenReturn(mockTruckDriverDetailsRequestV2);
        doNothing().when(modelMapper).map(inputConsolidation.getCarrierDetails(), mockCustomConsolidationRequest);
        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(new PartyRequestV2());
        when(modelMapper.map(any(), eq(JobRequestV2.class))).thenReturn(new JobRequestV2());
        when(modelMapper.map(any(), eq(FileRepoRequestV2.class))).thenReturn(new FileRepoRequestV2());

        doNothing().when(modelMapper).map(List.of(), new ArrayList<>());


        var responseEntity = consolidationSync.sync(inputConsolidation, guid.toString(), true);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }


    @Test
    void testSync5() throws RunnerException {
        // Arrange
        UserContext.setUser(UsersDto.builder().Username("dpwcanuser").build());
        TenantContext.setCurrentTenant(69);
        UUID guid = UUID.randomUUID();
        var inputConsolidation = new ConsolidationDetails();
        var inputContainer1 = new Containers();
        inputContainer1.setId(11L);
        inputContainer1.setGuid(UUID.randomUUID());

        var inputContainer2 = new Containers();
        inputContainer2.setId(12L);
        inputContainer2.setGuid(UUID.randomUUID());

        var inputJobs = new Jobs();
        inputJobs.setEventsList(List.of());

        inputConsolidation.setGuid(guid);
        inputConsolidation.setCarrierDetails(CarrierDetails.builder().build());
        inputConsolidation.setAchievedQuantities(AchievedQuantities.builder().build());
        inputConsolidation.setAllocations(Allocations.builder().build());
        inputConsolidation.setContainersList(List.of(inputContainer1, inputContainer2));
        inputConsolidation.setArrivalDetails(
                ArrivalDepartureDetails.builder()
                        .containerYardId(Parties.builder().build())
                        .firstForeignPortId(Parties.builder().build())
                        .lastForeignPortId(Parties.builder().build())
                        .transportPortId(Parties.builder().build())
                        .CFSId(Parties.builder().build())
                        .CTOId(Parties.builder().build())
                        .build());
        inputConsolidation.setDepartureDetails(
                ArrivalDepartureDetails.builder()
                        .containerYardId(Parties.builder().build())
                        .firstForeignPortId(Parties.builder().build())
                        .lastForeignPortId(Parties.builder().build())
                        .transportPortId(Parties.builder().build())
                        .CFSId(Parties.builder().build())
                        .CTOId(Parties.builder().build())
                        .build()
        );
        inputConsolidation.setFileRepoList(List.of(new FileRepo()));
        inputConsolidation.setCreditor(Parties.builder().isAddressFreeText(true).build());
        inputConsolidation.setReceivingAgent(Parties.builder().isAddressFreeText(true).build());
        inputConsolidation.setSendingAgent(Parties.builder().isAddressFreeText(true).build());
        inputConsolidation.setJobsList(List.of(inputJobs));
        inputConsolidation.setPackingList(List.of(new Packing()));
        var mockCustomConsolidationRequest = new CustomConsolidationRequest();
        var mockShipment = new ShipmentDetails();
        mockShipment.setId(11L);
        mockShipment.setGuid(UUID.randomUUID());
        mockShipment.setTenantId(1);

        var mockShipmentConsoleMapping = new ConsoleShipmentMapping();
        mockShipmentConsoleMapping.setConsolidationId(22L);
        mockShipmentConsoleMapping.setShipmentId(11L);

        var mockTruckDriverDetails1 = new TruckDriverDetails();
        mockTruckDriverDetails1.setShipmentId(11L);
        mockTruckDriverDetails1.setContainerId(11L);

        var mockTruckDriverDetails2 = new TruckDriverDetails();

        var mockTruckDriverDetailsRequestV2 = new TruckDriverDetailsRequestV2();

        when(modelMapper.map(any(), eq(CustomConsolidationRequest.class))).thenReturn(mockCustomConsolidationRequest);
        when(consoleShipmentMappingDao.findByConsolidationId(any())).thenReturn(List.of(mockShipmentConsoleMapping));
        when(shipmentDao.findShipmentsByIds(any())).thenReturn((List.of(mockShipment)));
        when(truckDriverDetailsDao.findAll(any(), any())).thenReturn(new PageImpl<>(List.of(mockTruckDriverDetails1, mockTruckDriverDetails2)));
        when(modelMapper.map(any(), eq(TruckDriverDetailsRequestV2.class))).thenReturn(mockTruckDriverDetailsRequestV2);
        doNothing().when(modelMapper).map(inputConsolidation.getCarrierDetails(), mockCustomConsolidationRequest);
        when(modelMapper.map(any(), eq(PartyRequestV2.class))).thenReturn(new PartyRequestV2());
        when(modelMapper.map(any(), eq(JobRequestV2.class))).thenReturn(new JobRequestV2());
        when(modelMapper.map(any(), eq(FileRepoRequestV2.class))).thenReturn(new FileRepoRequestV2());

        doNothing().when(modelMapper).map(List.of(), new ArrayList<>());

        var responseEntity = consolidationSync.sync(inputConsolidation, guid.toString(), false);

        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void testSync6() throws RunnerException {
        var responseEntity = consolidationSync.sync(null, UUID.randomUUID().toString(), false);

        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void syncLockStatus() {
        boolean isSuccess = true;
        var input = new ConsolidationDetails();
        input.setIsLocked(true);
        input.setGuid(UUID.randomUUID());
        consolidationSync.syncLockStatus(input);
        assertTrue(isSuccess);
    }

}