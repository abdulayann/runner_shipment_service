package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.CommonMocks;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.impl.QuartzJobInfoDao;
import com.dpw.runner.shipment.services.dao.interfaces.INetworkTransferDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.JobState;
import com.dpw.runner.shipment.services.entity.enums.NetworkTransferStatus;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.service.impl.QuartzJobInfoService;
import com.dpw.runner.shipment.services.service.interfaces.INetworkTransferService;
import com.dpw.runner.shipment.services.utils.NetworkTransferV3Util;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.commons.constants.Constants.*;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NetworkTransferV3UtilTest extends CommonMocks {

    @InjectMocks
    private NetworkTransferV3Util networkTransferV3Util;

    @Mock
    private INetworkTransferService networkTransferService;

    @Mock
    private INetworkTransferDao networkTransferDao;

    @Mock
    private QuartzJobInfoService quartzJobInfoService;

    @Mock
    private QuartzJobInfoDao quartzJobInfoDao;

    private static JsonTestUtility jsonTestUtility;
    private static ConsolidationDetails testConsol;


    @BeforeAll
    static void init() throws IOException{
        jsonTestUtility = new JsonTestUtility();
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        mockUser.setPermissions(new HashMap<>());
        UserContext.setUser(mockUser);
    }

    @BeforeEach
    void setup() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        testConsol = jsonTestUtility.getJson("CONSOLIDATION", ConsolidationDetails.class);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().airDGFlag(false).build());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer() {
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(1L).build();
        TriangulationPartner triangulationPartner1 = TriangulationPartner.builder().triangulationPartner(2L).build();
        TriangulationPartner triangulationPartner2 = TriangulationPartner.builder().triangulationPartner(3L).build();
        TriangulationPartner triangulationPartner3 = TriangulationPartner.builder().triangulationPartner(4L).build();
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setReceivingBranch(1L);
        shipmentDetails.setDirection("Inbound");
        shipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        shipmentDetails.setJobType(SHIPMENT_TYPE_DRT);
        shipmentDetails.setDirection(DIRECTION_EXP);
        shipmentDetails.setTriangulationPartnerList(List.of(triangulationPartner, triangulationPartner1, triangulationPartner2));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setReceivingBranch(1L);
        oldEntity.setTriangulationPartnerList(List.of(triangulationPartner2, triangulationPartner3));
        when(commonUtils.getTriangulationPartnerList(shipmentDetails.getTriangulationPartnerList())).thenReturn(List.of(1L, 2L, 3L));
        when(commonUtils.getTriangulationPartnerList(oldEntity.getTriangulationPartnerList())).thenReturn(List.of(3L, 4L));
        // Act
        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentDetails, oldEntity);

        // Verify new tenant IDs processing
        verify(networkTransferService, times(1)).processNetworkTransferEntity(eq(1L), isNull(), eq(Constants.SHIPMENT), eq(shipmentDetails), isNull(), eq(Constants.DIRECTION_CTS), isNull(), anyBoolean());
        verify(networkTransferService, times(1)).processNetworkTransferEntity(eq(2L), isNull(), eq(Constants.SHIPMENT), eq(shipmentDetails), isNull(), eq(Constants.DIRECTION_CTS), isNull(), anyBoolean());

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).processNetworkTransferEntity(isNull(), eq(4L), eq(Constants.SHIPMENT), eq(shipmentDetails), isNull(), eq(Constants.DIRECTION_CTS), isNull(), anyBoolean());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_NotEligibleForNetworkTransfer() {
        TriangulationPartner triangulationPartner = TriangulationPartner.builder().triangulationPartner(3L).build();
        TriangulationPartner triangulationPartner1 = TriangulationPartner.builder().triangulationPartner(4L).build();
        TriangulationPartner triangulationPartner2 = TriangulationPartner.builder().triangulationPartner(5L).build();
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setReceivingBranch(1L);
        shipmentDetails.setDirection("NonEligibleDirection"); // Non-eligible direction
        shipmentDetails.setTransportMode("NonEligibleTransportMode"); // Non-eligible transport mode
        shipmentDetails.setJobType("NonEligibleJobType"); // Non-eligible job type

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setId(100L); // Mocked ID for oldEntity
        oldEntity.setReceivingBranch(2L); // Old receiving branch
        oldEntity.setTriangulationPartnerList(List.of(triangulationPartner, triangulationPartner1, triangulationPartner2)); // Old triangulation partners

        // Act
        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentDetails, oldEntity);

        // Assert and Verify

        // Verify that deleteValidNetworkTransferEntity is called for oldEntity's receivingBranch
        verify(networkTransferService, times(1))
                .deleteValidNetworkTransferEntity(2L, 100L, Constants.SHIPMENT);

        // Verify that deleteValidNetworkTransferEntity is called for each triangulation partner
        verify(networkTransferService, times(1))
                .deleteValidNetworkTransferEntity(3L, 100L, Constants.SHIPMENT);
        verify(networkTransferService, times(1))
                .deleteValidNetworkTransferEntity(4L, 100L, Constants.SHIPMENT);
        verify(networkTransferService, times(1))
                .deleteValidNetworkTransferEntity(5L, 100L, Constants.SHIPMENT);

        // Ensure no processNetworkTransferEntity is invoked
        verify(networkTransferService, never()).processNetworkTransferEntity(any(), any(), any(), any(), any(), any(), any(), any());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_InterConsole() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setReceivingBranch(1L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setReceivingBranch(2L);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setReceivingBranch(1L);

        // Act
        networkTransferV3Util.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).processNetworkTransferEntity(eq(1L), isNull(), eq(Constants.SHIPMENT), eq(newShipmentDetails), isNull(), eq(IMP), isNull(), eq(true));
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_OldInterConsole() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setReceivingBranch(1L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(NetworkTransfer.builder().build()));

        // Act
        networkTransferV3Util.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).deleteNetworkTransferEntity(any());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_InterConsole2() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setReceivingBranch(2L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        consolidationDetails1.setReceivingBranch(1L);
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        // Act
        networkTransferV3Util.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).processNetworkTransferEntity(eq(2L), eq(1L), eq(Constants.SHIPMENT), eq(newShipmentDetails), isNull(), eq(IMP), isNull(), eq(true));
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_OldInterConsole2() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setId(1L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        consolidationDetails1.setReceivingBranch(1L);
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(NetworkTransfer.builder().entityId(1L).isInterBranchEntity(true).build()));

        // Act
        networkTransferV3Util.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).deleteNetworkTransferEntity(any());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_InterConsole3() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setId(1L);
        newShipmentDetails.setReceivingBranch(2L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setReceivingBranch(2L);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).isInterBranchEntity(true).build()));

        // Act
        networkTransferV3Util.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferDao, times(1)).save(any());
        verify(networkTransferService, times(1)).deleteNetworkTransferEntity(any());
    }

    @Test
    void testCreateOrUpdateNetworkTransferEntity_EligibleForNetworkTransfer_InterConsole4() {
        // Arrange
        ShipmentDetails newShipmentDetails = new ShipmentDetails();
        newShipmentDetails.setId(1L);
        newShipmentDetails.setReceivingBranch(2L);
        newShipmentDetails.setDirection("Inbound");
        newShipmentDetails.setTransportMode(TRANSPORT_MODE_AIR);
        newShipmentDetails.setJobType(SHIPMENT_TYPE_STD);
        newShipmentDetails.setDirection(DIRECTION_EXP);
        ConsolidationDetails consolidationDetails1 = testConsol;
        consolidationDetails1.setInterBranchConsole(true);
        consolidationDetails1.setReceivingBranch(2L);
        consolidationDetails1.setShipmentsList(new HashSet<>(Collections.singletonList(newShipmentDetails)));
        newShipmentDetails.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        ShipmentDetails oldEntity = new ShipmentDetails();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(1L);
        oldEntity.setConsolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1)));

        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(null);

        // Act
        networkTransferV3Util.createOrUpdateNetworkTransferEntity(newShipmentDetails, oldEntity);

        // Verify old tenant IDs processing for removal
        verify(networkTransferService, times(1)).bulkProcessInterConsoleNte(any());
    }

    @Test
    void triggerAutomaticTransferTest() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        when(quartzJobInfoService.getQuartzJobTime(any(),any(),any(),any(), any())).thenReturn(LocalDateTime.now());
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, null, false);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest13() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(null).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        ShipmentDetails oldEntity = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, oldEntity, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest2() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest3() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.TRANSFERRED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest4() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest5() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.QUEUED).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, false);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest6() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        when(quartzJobInfoService.getQuartzJobTime(any(), any(), any(), any(), any())).thenReturn(LocalDateTime.now());
        when(quartzJobInfoDao.save(any())).thenReturn(quartzJobInfo);
        when(quartzJobInfoService.isJobWithNamePresent(any())).thenReturn(true);
        when(quartzJobInfoService.updateSimpleJob(any())).thenReturn(null);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest7() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest8() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.QUEUED).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest9() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.QUEUED).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.empty());
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(0)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest10() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(LocalDateTime.now().plusHours(4)).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        when(quartzJobInfoService.getQuartzJobTime(any(), any(), any(), any(), any())).thenReturn(LocalDateTime.now());
        when(quartzJobInfoDao.save(any())).thenReturn(quartzJobInfo);
        when(quartzJobInfoService.isJobWithNamePresent(any())).thenReturn(false);
        when(quartzJobInfoService.createSimpleJob(any())).thenReturn(null);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails2, true);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest11() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        var dateTime = LocalDateTime.now().plusHours(4);
        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(dateTime).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        ShipmentDetails shipmentDetails3 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).
                carrierDetails(CarrierDetails.builder().eta(dateTime).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        when(quartzJobInfoService.getQuartzJobTime(any(), any(), any(), any(), any())).thenReturn(LocalDateTime.now());
        when(quartzJobInfoDao.save(any())).thenReturn(quartzJobInfo);
        when(quartzJobInfoService.isJobWithNamePresent(any())).thenReturn(false);
        when(quartzJobInfoService.createSimpleJob(any())).thenReturn(null);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails3, false);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest12() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        var dateTime = LocalDateTime.now().plusHours(4);
        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).
                carrierDetails(CarrierDetails.builder().eta(dateTime).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        ShipmentDetails shipmentDetails3 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType(Constants.SHIPMENT_TYPE_DRT).
                direction(Constants.DIRECTION_EXP).receivingBranch(2L).
                carrierDetails(CarrierDetails.builder().eta(dateTime).build()).
                additionalDetails(new AdditionalDetails()).consolidationList(new HashSet<>()).
                containersList(new HashSet<>()).triangulationPartner(12L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));
        when(quartzJobInfoService.getQuartzJobTime(any(), any(), any(), any(), any())).thenReturn(LocalDateTime.now());
        when(quartzJobInfoDao.save(any())).thenReturn(quartzJobInfo);
        List<V1TenantSettingsResponse.FileTransferConfigurations> fileTransferConfigurationsList = Collections.singletonList(V1TenantSettingsResponse.FileTransferConfigurations.builder().build());
        when(quartzJobInfoService.getActiveFileTransferConfigurations(any())).thenReturn(fileTransferConfigurationsList);
        when(quartzJobInfoService.isJobWithNamePresent(any())).thenReturn(false);
        when(quartzJobInfoService.createSimpleJob(any())).thenReturn(null);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().status(NetworkTransferStatus.SCHEDULED).build();
        when(networkTransferDao.findByTenantAndEntity(any(), any(), any())).thenReturn(Optional.of(networkTransfer));
        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, shipmentDetails3, false);

        verify(quartzJobInfoService, times(1)).getQuartzJobTime(any(), any(), any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest14() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder().build();
        consolidationDetails1.setTransportMode(TRANSPORT_MODE_SEA);

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType("FCT").
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).masterBill("ABCD").
                consolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1))).build();

        ShipmentDetails oldEntity = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType("FCT").
                direction(Constants.DIRECTION_EXP).receivingBranch(2L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));

        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, oldEntity, false);

        verify(quartzJobInfoDao, times(2)).findByJobFilters(any(), any(), any());
    }

    @Test
    void triggerAutomaticTransferTest15() {
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().autoEventCreate(false).isNetworkTransferEntityEnabled(true).isAutomaticTransferEnabled(true).build());

        ConsolidationDetails consolidationDetails1 = ConsolidationDetails.builder().build();
        consolidationDetails1.setTransportMode(TRANSPORT_MODE_AIR);

        ShipmentDetails shipmentDetails2 = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType("FCT").
                direction(Constants.DIRECTION_EXP).receivingBranch(1L).houseBill("ABCD").
                consolidationList(new HashSet<>(Collections.singletonList(consolidationDetails1))).build();

        ShipmentDetails oldEntity = ShipmentDetails.builder().shipmentId("AIR-CAN-00001").
                transportMode(TRANSPORT_MODE_AIR).jobType("FCT").
                direction(Constants.DIRECTION_EXP).receivingBranch(2L).build();

        QuartzJobInfo quartzJobInfo = QuartzJobInfo.builder().jobStatus(JobState.ERROR).build();
        quartzJobInfo.setId(1L);
        when(quartzJobInfoDao.findByJobFilters(any(), any(), any())).thenReturn(Optional.of(quartzJobInfo));

        networkTransferV3Util.triggerAutomaticTransfer(shipmentDetails2, oldEntity, false);

        verify(quartzJobInfoDao, times(2)).findByJobFilters(any(), any(), any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ShipmentDetails sDetails = ShipmentDetails.builder().receivingBranch(2L).build();
        sDetails.setId(1L);
        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(1L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(sDetails)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferService, times(1)).processNetworkTransferEntity(any(), any(),any(),any(),any(),any(),any(),any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest_2(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(1L);

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);
        oldEntity.setShipmentsList(new HashSet<>(Collections.singletonList(ShipmentDetails.builder().receivingBranch(2L).build())));


        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferService, times(1)).deleteValidNetworkTransferEntity(any(), any(),any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest_4(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(1L);
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().receivingBranch(2L).build();
        shipmentDetails1.setId(1L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(shipmentDetails1)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferService, times(1)).processNetworkTransferEntity(any(), any(),any(), any(), any(),any(), any(), any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest_5(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ShipmentDetails sDetails = ShipmentDetails.builder().receivingBranch(2L).build();
        sDetails.setId(1L);
        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(1L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(sDetails)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);

        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferService, times(1)).processNetworkTransferEntity(any(), any(),any(),any(),any(),any(),any(),any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest_6(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(1L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(ShipmentDetails.builder().receivingBranch(2L).build())));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);

        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(null);

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferService, times(2)).processNetworkTransferEntity(any(), any(),any(),any(),any(),any(),any(),any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest_7(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ShipmentDetails sDetails = ShipmentDetails.builder().receivingBranch(2L).build();
        sDetails.setId(1L);

        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(2L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(sDetails)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(1L);

        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferService, times(1)).processNetworkTransferEntity(any(), any(),any(),any(),any(),any(),any(),any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest_8(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(1L);
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().receivingBranch(2L).build();
        shipmentDetails1.setId(1L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(shipmentDetails1)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).status(NetworkTransferStatus.REASSIGNED).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferService, times(1)).processNetworkTransferEntity(any(), any(),any(), any(), any(),any(), any(), any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest_9(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        ShipmentDetails shipmentDetails1 = ShipmentDetails.builder().receivingBranch(2L).build();
        shipmentDetails1.setId(1L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(shipmentDetails1)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).status(NetworkTransferStatus.REASSIGNED).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferService, times(1)).processNetworkTransferEntity(any(), any(),any(), any(), any(),any(), any(), any());
        verify(networkTransferService, times(1)).deleteNetworkTransferEntity(any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest10(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ShipmentDetails sDetails = ShipmentDetails.builder().receivingBranch(2L).build();
        sDetails.setId(1L);
        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(false);
        consoleDetails.setReceivingBranch(1L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(sDetails)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);
        oldEntity.setInterBranchConsole(true);

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).status(NetworkTransferStatus.ACCEPTED).entityPayload(entityPayload).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferDao, times(1)).getInterConsoleNTList(any(), any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest11(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ShipmentDetails sDetails = ShipmentDetails.builder().receivingBranch(2L).build();
        sDetails.setId(1L);
        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(false);
        consoleDetails.setReceivingBranch(1L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(sDetails)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);
        oldEntity.setInterBranchConsole(true);

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(Collections.singletonList(networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferDao, times(2)).getInterConsoleNTList(any(), any());
        verify(networkTransferService, times(1)).deleteNetworkTransferEntity(any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest12(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ShipmentDetails sDetails = ShipmentDetails.builder().receivingBranch(2L).build();
        sDetails.setId(1L);
        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(2L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(sDetails)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);
        oldEntity.setInterBranchConsole(false);

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        NetworkTransfer networkTransfer2 = NetworkTransfer.builder().entityId(1L).jobType(DIRECTION_CTS).status(NetworkTransferStatus.ACCEPTED).isInterBranchEntity(true).build();
        networkTransfer2.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(List.of(networkTransfer2, networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferDao, times(1)).save(any());
    }

    @Test
    void createOrUpdateNetworkTransferEntityTest13(){
        ShipmentSettingsDetails shipmentSettingsDetails = ShipmentSettingsDetailsContext.getCurrentTenantSettings();
        shipmentSettingsDetails.setIsNetworkTransferEntityEnabled(true);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(shipmentSettingsDetails);

        ShipmentDetails sDetails = ShipmentDetails.builder().receivingBranch(2L).build();
        sDetails.setId(1L);
        ConsolidationDetails consoleDetails = jsonTestUtility.getTestConsolidationAir();
        consoleDetails.setId(1L);
        consoleDetails.setInterBranchConsole(true);
        consoleDetails.setReceivingBranch(2L);
        consoleDetails.setShipmentsList(new HashSet<>(Collections.singletonList(sDetails)));

        ConsolidationDetails oldEntity = jsonTestUtility.getTestConsolidationAir();
        oldEntity.setId(1L);
        oldEntity.setReceivingBranch(2L);
        oldEntity.setInterBranchConsole(false);

        Map<String, Object> entityPayload = Map.of("abcd", 1);
        NetworkTransfer networkTransfer = NetworkTransfer.builder().entityId(1L).entityPayload(entityPayload).status(NetworkTransferStatus.ACCEPTED).isInterBranchEntity(true).build();
        networkTransfer.setTenantId(2);
        NetworkTransfer networkTransfer2 = NetworkTransfer.builder().entityId(1L).jobType(DIRECTION_CTS).status(NetworkTransferStatus.ACCEPTED).isInterBranchEntity(true).build();
        networkTransfer2.setTenantId(2);
        when(networkTransferDao.getInterConsoleNTList(any(), any())).thenReturn(List.of(networkTransfer2, networkTransfer));

        networkTransferV3Util.createOrUpdateNetworkTransferEntity(shipmentSettingsDetails, consoleDetails, oldEntity);

        verify(networkTransferDao, times(1)).getInterConsoleNTList(any(), any());
    }

}
