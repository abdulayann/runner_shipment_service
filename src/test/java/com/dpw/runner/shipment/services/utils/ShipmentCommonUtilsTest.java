package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.AdditionalDetailResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.notification.PendingShipmentActionsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1RetrieveResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentRequestedType;
import com.dpw.runner.shipment.services.entity.enums.TaskStatus;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.ResponseEntity;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class ShipmentCommonUtilsTest {

    @Mock
    private CommonUtils commonUtils;
    @Mock
    private IV1Service v1Service;
    @Mock
    private ProductIdentifierUtility productEngine;
    @Mock
    private ModelMapper modelMapper;
    @Mock
    private MasterDataUtils masterDataUtils;
    @Mock
    private V1ServiceUtil v1ServiceUtil;
    @Mock
    private IAuditLogService auditLogService;
    @Mock
    private JsonHelper jsonHelper;
    @Mock
    private IShipmentDao shipmentDao;

    @InjectMocks
    private ShipmentCommonUtils shipmentCommonUtils;

    private ShipmentSettingsDetails shipmentSettings;
    private UsersDto user;

    @BeforeEach
    void setUp() {
        shipmentSettings = ShipmentSettingsDetails.builder()
                .defaultTransportMode(Constants.TRANSPORT_MODE_SEA)
                .defaultShipmentType(Constants.DIRECTION_EXP)
                .defaultContainerType("FCL")
                .defaultPackUnit("PKG")
                .weightChargeableUnit("KGS")
                .volumeChargeableUnit("M3")
                .restrictHblGen(false)
                .customisedSequence(false)
                .housebillPrefix("HB")
                .housebillNumberGeneration("Serial")
                .isRunnerV3Enabled(true)
                .build();

        lenient().when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettings);

        user = new UsersDto();
        user.setTenantId(1);
        user.setUsername("user1");
        UserContext.setUser(user);
    }

    // ---------- TEST determineOperationTypeAfterApproval ----------

    @Test
    void testDetermineOperationTypeAfterApproval_DGRequestedApproved() {
        TaskStatus status = TaskStatus.APPROVED;
        DBOperationType result = shipmentCommonUtils.determineOperationTypeAfterApproval(
                OceanDGStatus.OCEAN_DG_REQUESTED, () -> status);
        assertEquals(DBOperationType.DG_APPROVE, result);
    }

    @Test
    void testDetermineOperationTypeAfterApproval_DGRequestedRejected() {
        TaskStatus status = TaskStatus.REJECTED;
        DBOperationType result = shipmentCommonUtils.determineOperationTypeAfterApproval(
                OceanDGStatus.OCEAN_DG_REQUESTED, () -> status);
        assertEquals(DBOperationType.DG_REJECT, result);
    }

    @Test
    void testDetermineOperationTypeAfterApproval_CommercialRequestedApproved() {
        TaskStatus status = TaskStatus.APPROVED;
        DBOperationType result = shipmentCommonUtils.determineOperationTypeAfterApproval(
                OceanDGStatus.OCEAN_DG_COMMERCIAL_REQUESTED, () -> status);
        assertEquals(DBOperationType.COMMERCIAL_APPROVE, result);
    }

    @Test
    void testDetermineOperationTypeAfterApproval_DefaultCase() {
        DBOperationType result = shipmentCommonUtils.determineOperationTypeAfterApproval(
                null, () -> TaskStatus.REJECTED);
        assertEquals(DBOperationType.DG_REQUEST, result);
    }

    // ---------- TEST setExportBrokerForInterBranchConsole ----------


    @Test
    void testSetExportBrokerForInterBranchConsole_NullAdditionalDetails() {
        ShipmentDetails shipment = new ShipmentDetails();
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setSendingAgent(new Parties());

        when(commonUtils.removeIdFromParty(any())).thenReturn(new Parties());

        shipmentCommonUtils.setExportBrokerForInterBranchConsole(shipment, consolidation);
        assertNotNull(shipment.getAdditionalDetails());
    }


    // ---------- TEST getShipmentsSerialNumber ----------

    @Test
    void testGetShipmentsSerialNumber_Success() {
        when(v1Service.getShipmentSerialNumber()).thenReturn("12345");
        assertEquals("12345", shipmentCommonUtils.getShipmentsSerialNumber());
    }

    // ---------- TEST generateCustomHouseBL ----------

    @Test
    void testGenerateCustomHouseBL_CustomizedSequenceTrue() throws RunnerException {
        shipmentSettings.setCustomisedSequence(true);
        ShipmentDetails shipment = new ShipmentDetails();
        shipment.setHouseBill("HB123");
        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettings);
        when(productEngine.getCustomizedBLNumber(any())).thenReturn("CUST123");

        String result = shipmentCommonUtils.generateCustomHouseBL(shipment);
        assertEquals("CUST123", result);
    }

    @Test
    void testGenerateCustomHouseBL_RandomGeneration() {
        shipmentSettings.setCustomisedSequence(false);
        shipmentSettings.setHousebillNumberGeneration("Random");
        String result = shipmentCommonUtils.generateCustomHouseBL(new ShipmentDetails());
        assertTrue(result.startsWith("HB"));
    }

    @Test
    void testGenerateCustomHouseBL_SerialGeneration() {
        shipmentSettings.setCustomisedSequence(false);
        shipmentSettings.setHousebillNumberGeneration("Serial");
        when(v1Service.getShipmentSerialNumber()).thenReturn("1001");

        String result = shipmentCommonUtils.generateCustomHouseBL(new ShipmentDetails());
        assertEquals("HB1001", result);
    }

    @Test
    void testGenerateCustomHouseBL_NullShipmentRestrictTrue() {
        shipmentSettings.setRestrictHblGen(true);
        String result = shipmentCommonUtils.generateCustomHouseBL(null);
        assertNull(result);
    }

    // ---------- TEST getIdFromGuid ----------

    @Test
    void testGetIdFromGuid_Success() {
        CommonGetRequest req = new CommonGetRequest();
        req.setGuid(UUID.randomUUID().toString());

        ShipmentDetails details = new ShipmentDetails();
        details.setId(10L);
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.of(details));

        CommonRequestModel model = CommonRequestModel.builder().build();
        model.setData(req);

        ResponseEntity<IRunnerResponse> response = shipmentCommonUtils.getIdFromGuid(model);
        assertEquals(200, response.getStatusCodeValue());
    }

    @Test
    void testGetIdFromGuid_DataNotFound() {
        CommonGetRequest req = new CommonGetRequest();
        req.setGuid(UUID.randomUUID().toString());
        when(shipmentDao.findByGuid(any())).thenReturn(Optional.empty());

        CommonRequestModel model = CommonRequestModel.builder().build();
        model.setData(req);

        ResponseEntity<IRunnerResponse> response = shipmentCommonUtils.getIdFromGuid(model);
        assertEquals(400, response.getStatusCodeValue());
    }

    // ---------- TEST mapToNotification ----------

    @Test
    void testMapToNotification_WithRequestedType() {
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setId(1L);
        consolidation.setTenantId(1);
        consolidation.setCarrierDetails(new CarrierDetails());
        consolidation.setReferenceNumber("CON001");

        ConsoleShipmentMapping mapping = new ConsoleShipmentMapping();
        mapping.setCreatedBy("user1");
        mapping.setCreatedAt(LocalDateTime.now());
        mapping.setRequestedType(ShipmentRequestedType.SHIPMENT_PUSH_REJECTED);

        Map<Long, ConsoleShipmentMapping> map = Map.of(1L, mapping);
        Map<String, TenantModel> tenantMap = Map.of("1", new TenantModel());
        Map<String, EntityTransferUnLocations> locMap = new HashMap<>();

        PendingShipmentActionsResponse response = shipmentCommonUtils.mapToNotification(consolidation, map, tenantMap, locMap, true);
        assertEquals("user1", response.getRequestedBy());
    }

    // ---------- TEST createAuditLog ----------

    @Test
    void testCreateAuditLog_Success() throws Exception {
        ShipmentDetails details = new ShipmentDetails();
        details.setId(10L);
        when(jsonHelper.readFromJson(any(), eq(ShipmentDetails.class))).thenReturn(new ShipmentDetails());
        doNothing().when(auditLogService).addAuditLog(any());

        shipmentCommonUtils.createAuditLog(details, "{}", "UPDATE");
        verify(auditLogService).addAuditLog(any());
    }

    @Test
    void testCreateAuditLog_Exception() throws Exception {
        ShipmentDetails details = new ShipmentDetails();
        doThrow(new RuntimeException("FAIL")).when(auditLogService).addAuditLog(any());
        shipmentCommonUtils.createAuditLog(details, null, "INSERT");
        assertNotNull(details);
    }

    @Test
    void testSetDefaultAgentAndTenant_Success_Export() {
        ShipmentDetailsResponse response = new ShipmentDetailsResponse();
        response.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        response.setDirection(Constants.DIRECTION_EXP);
        response.setAdditionalDetails(new AdditionalDetailResponse());

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsRunnerV3Enabled(true);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCurrencyCode("USD");
        tenantModel.setUnloco(100);

        UnlocationsResponse unlocResponse = new UnlocationsResponse();
        unlocResponse.setLocationsReferenceGUID("LOC123");
        List<UnlocationsResponse> unlocList = Collections.singletonList(unlocResponse);

        EntityTransferAddress entityTransferAddress = new EntityTransferAddress();
        entityTransferAddress.setCity("Mumbai");

        PartiesResponse partiesResponse = new PartiesResponse();
        partiesResponse.setOrgId("1L");
        partiesResponse.setAddressId("2L");

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
        when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(masterDataUtils.fetchUnlocationByOneIdentifier(anyString(), anyString())).thenReturn(unlocList);
        when(commonUtils.getEntityTransferAddress(tenantModel)).thenReturn(entityTransferAddress);
        when(v1ServiceUtil.getDefaultAgentOrg(tenantModel)).thenReturn(partiesResponse);
        when(commonUtils.getReceivingBranch(anyString(), anyString())).thenReturn(10L);

        shipmentCommonUtils.setDefaultAgentAndTenant(response);

        assertEquals("USD", response.getFreightLocalCurrency());
        assertEquals("Mumbai", response.getAdditionalDetails().getPlaceOfIssue());
        assertEquals("LOC123", response.getAdditionalDetails().getPlaceOfSupply());
        assertEquals("LOC123", response.getAdditionalDetails().getPaidPlace());
        assertNotNull(response.getAdditionalDetails().getExportBroker());
    }

    @Test
    void testSetDefaultAgentAndTenant_Success_Import() {
        ShipmentDetailsResponse response = new ShipmentDetailsResponse();
        response.setTransportMode(Constants.TRANSPORT_MODE_AIR);
        response.setDirection(Constants.DIRECTION_IMP);
        response.setAdditionalDetails(new AdditionalDetailResponse());

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        shipmentSettingsDetails.setIsRunnerV3Enabled(false);
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCurrencyCode("EUR");
        tenantModel.setUnloco(200);

        UnlocationsResponse unlocResponse = new UnlocationsResponse();
        unlocResponse.setLocationsReferenceGUID("LOC456");
        List<UnlocationsResponse> unlocList = Collections.singletonList(unlocResponse);

        PartiesResponse partiesResponse = new PartiesResponse();

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
        when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(masterDataUtils.fetchUnlocationByOneIdentifier(anyString(), anyString())).thenReturn(unlocList);
        when(v1ServiceUtil.getDefaultAgentOrg(tenantModel)).thenReturn(partiesResponse);

        shipmentCommonUtils.setDefaultAgentAndTenant(response);

        assertEquals("EUR", response.getFreightLocalCurrency());
        assertEquals("LOC456", response.getAdditionalDetails().getPlaceOfIssue());
        assertEquals("LOC456", response.getAdditionalDetails().getPlaceOfSupply());
        assertEquals("LOC456", response.getAdditionalDetails().getPaidPlace());
        assertNotNull(response.getAdditionalDetails().getImportBroker());
    }

    @Test
    void testSetDefaultAgentAndTenant_NoUnlocationData() {
        ShipmentDetailsResponse response = new ShipmentDetailsResponse();
        response.setTransportMode(Constants.TRANSPORT_MODE_SEA);
        response.setDirection(Constants.DIRECTION_EXP);
        response.setAdditionalDetails(new AdditionalDetailResponse());

        ShipmentSettingsDetails shipmentSettingsDetails = new ShipmentSettingsDetails();
        TenantModel tenantModel = new TenantModel();
        tenantModel.setCurrencyCode("INR");

        when(commonUtils.getShipmentSettingFromContext()).thenReturn(shipmentSettingsDetails);
        when(v1Service.retrieveTenant()).thenReturn(new V1RetrieveResponse());
        when(modelMapper.map(any(), eq(TenantModel.class))).thenReturn(tenantModel);
        when(masterDataUtils.fetchUnlocationByOneIdentifier(anyString(), anyString())).thenReturn(Collections.emptyList());

        shipmentCommonUtils.setDefaultAgentAndTenant(response);

        assertEquals("INR", response.getFreightLocalCurrency());
    }

    @Test
    void testSetDefaultAgentAndTenant_Exception() {
        ShipmentDetailsResponse response = new ShipmentDetailsResponse();
        response.setAdditionalDetails(new AdditionalDetailResponse());

        when(commonUtils.getShipmentSettingFromContext()).thenThrow(new RuntimeException("FAIL"));

        shipmentCommonUtils.setDefaultAgentAndTenant(response);

        // No exception should propagate — just log error
        assertNull(response.getFreightLocalCurrency());
    }

    @Test
    void testAddAuditLogContainers_Success() throws Exception {
        Containers newContainer = new Containers();
        Containers prevContainer = new Containers();
        Long shipmentId = 1001L;
        String operationName = "UPDATE";


        doNothing().when(auditLogService).addAuditLog(any(AuditLogMetaData.class));

        shipmentCommonUtils.addAuditLogContainers(newContainer, prevContainer, shipmentId, operationName);

        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    @Test
    void testAddAuditLogContainers_Exception() throws Exception {
        Containers newContainer = new Containers();
        Containers prevContainer = new Containers();
        Long shipmentId = 2002L;
        String operationName = "DELETE";

        doThrow(new IllegalAccessException("ACCESS_FAIL")).when(auditLogService).addAuditLog(any());

        shipmentCommonUtils.addAuditLogContainers(newContainer, prevContainer, shipmentId, operationName);

        verify(auditLogService, times(1)).addAuditLog(any());
    }

    @Test
    void testAddAuditLogContainers_NullValues() throws Exception {
        doNothing().when(auditLogService).addAuditLog(any());

        // When everything except operationName is null
        shipmentCommonUtils.addAuditLogContainers(null, null, null, "INSERT");

        verify(auditLogService, times(1)).addAuditLog(any(AuditLogMetaData.class));
    }

    // Test build Default Shipment  ----------------------------------------

    @Test
    void testBuildDefaultShipment_ExceptionInDepartmentFetch() {
        ShipmentCommonUtils spy = Mockito.spy(shipmentCommonUtils);
        ShipmentSettingsDetails settings = new ShipmentSettingsDetails();
        settings.setDefaultTransportMode(Constants.TRANSPORT_MODE_SEA);
        settings.setDefaultShipmentType(Constants.DIRECTION_EXP);
        settings.setDefaultContainerType("FCL");
        settings.setWeightChargeableUnit("KG");
        settings.setDefaultPackUnit("CTN");

        lenient().doNothing().when(spy).setDefaultAgentAndTenant(any());
        doReturn("ASD").when(commonUtils).getAutoPopulateDepartment(anyString(), anyString(), anyString());
        doReturn("123").when(spy).generateCustomHouseBL(any());
        doReturn(settings).when(commonUtils).getShipmentSettingFromContext();


        // Should not throw exception
        ShipmentDetailsResponse response = spy.buildDefaultShipment();

        assertNotNull(response);
        assertEquals(Constants.DIRECTION_EXP, response.getDirection());
        verify(commonUtils).getShipmentSettingFromContext();
    }

    //Test export Broker
    @Test
    void testSetExportBrokerForInterBranchConsole_WhenAdditionalDetailsPresentAndDifferentParties() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        Parties currentExportBroker = new Parties();
        additionalDetails.setExportBroker(currentExportBroker);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        Parties sendingAgent = new Parties();
        consolidationDetails.setSendingAgent(sendingAgent);

        when(commonUtils.removeIdFromParty(sendingAgent)).thenReturn(sendingAgent);

        // ✅ Proper static mocking (auto-closes)
        try (MockedStatic<CommonUtils> mockedCommonUtils = mockStatic(CommonUtils.class)) {
            mockedCommonUtils.when(() -> CommonUtils.checkSameParties(sendingAgent, currentExportBroker))
                    .thenReturn(false);

            shipmentCommonUtils.setExportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);

            assertEquals(sendingAgent, shipmentDetails.getAdditionalDetails().getExportBroker());
            verify(commonUtils).removeIdFromParty(sendingAgent);
        }
    }


    @Test
    void testSetExportBrokerForInterBranchConsole_WhenAdditionalDetailsNull() {
        ShipmentDetails shipmentDetails = new ShipmentDetails(); // no additionalDetails
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        Parties sendingAgent = new Parties();
        consolidationDetails.setSendingAgent(sendingAgent);

        when(commonUtils.removeIdFromParty(sendingAgent)).thenReturn(sendingAgent);

        shipmentCommonUtils.setExportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);

        assertNotNull(shipmentDetails.getAdditionalDetails());
        assertEquals(sendingAgent, shipmentDetails.getAdditionalDetails().getExportBroker());
        verify(commonUtils).removeIdFromParty(sendingAgent);
    }

    @Test
    void testSetExportBrokerForInterBranchConsole_WhenSameParties_NoChange() {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        Parties exportBroker = new Parties();
        additionalDetails.setExportBroker(exportBroker);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        Parties sendingAgent = new Parties();
        consolidationDetails.setSendingAgent(sendingAgent);

        // ✅ Properly close the static mock
        try (MockedStatic<CommonUtils> mockedCommonUtils = mockStatic(CommonUtils.class)) {
            mockedCommonUtils.when(() -> CommonUtils.checkSameParties(sendingAgent, exportBroker))
                    .thenReturn(true);

            shipmentCommonUtils.setExportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);

            assertEquals(exportBroker, shipmentDetails.getAdditionalDetails().getExportBroker());
            verifyNoInteractions(commonUtils);
        }
    }

    @Test
    void testSetImportBrokerForInterBranchConsole_WhenAdditionalDetailsPresentAndDifferentParties() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        Parties currentImportBroker = new Parties();
        additionalDetails.setImportBroker(currentImportBroker);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        Parties receivingAgent = new Parties();
        consolidationDetails.setReceivingAgent(receivingAgent);

        when(commonUtils.removeIdFromParty(receivingAgent)).thenReturn(receivingAgent);

        // Act + Assert
        try (MockedStatic<CommonUtils> mockedCommonUtils = mockStatic(CommonUtils.class)) {
            mockedCommonUtils.when(() -> CommonUtils.checkSameParties(receivingAgent, currentImportBroker))
                    .thenReturn(false);

            shipmentCommonUtils.setImportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);

            assertEquals(receivingAgent, shipmentDetails.getAdditionalDetails().getImportBroker());
            verify(commonUtils).removeIdFromParty(receivingAgent);
        }
    }

    @Test
    void testSetImportBrokerForInterBranchConsole_WhenAdditionalDetailsIsNull() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails(); // no AdditionalDetails
        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        Parties receivingAgent = new Parties();
        consolidationDetails.setReceivingAgent(receivingAgent);

        when(commonUtils.removeIdFromParty(receivingAgent)).thenReturn(receivingAgent);

        // Act
        shipmentCommonUtils.setImportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);

        // Assert
        assertNotNull(shipmentDetails.getAdditionalDetails());
        assertEquals(receivingAgent, shipmentDetails.getAdditionalDetails().getImportBroker());
        verify(commonUtils).removeIdFromParty(receivingAgent);
    }

    @Test
    void testSetImportBrokerForInterBranchConsole_WhenSameParties_NoChange() {
        // Arrange
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        AdditionalDetails additionalDetails = new AdditionalDetails();
        Parties importBroker = new Parties();
        additionalDetails.setImportBroker(importBroker);
        shipmentDetails.setAdditionalDetails(additionalDetails);

        ConsolidationDetails consolidationDetails = new ConsolidationDetails();
        Parties receivingAgent = new Parties();
        consolidationDetails.setReceivingAgent(receivingAgent);

        // Act + Assert
        try (MockedStatic<CommonUtils> mockedCommonUtils = mockStatic(CommonUtils.class)) {
            mockedCommonUtils.when(() -> CommonUtils.checkSameParties(receivingAgent, importBroker))
                    .thenReturn(true);

            shipmentCommonUtils.setImportBrokerForInterBranchConsole(shipmentDetails, consolidationDetails);

            assertEquals(importBroker, shipmentDetails.getAdditionalDetails().getImportBroker());
            verifyNoInteractions(commonUtils);
        }
    }

}

