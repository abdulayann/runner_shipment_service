package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.SailingInformationResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class CarrierBookingInttraUtilTest {

    @InjectMocks
    private CarrierBookingInttraUtil carrierBookingInttraUtil;

    @Mock
    private ITransactionHistoryDao transactionHistoryDao;

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private BridgeServiceAdapter bridgeServiceAdapter;

    @Mock
    private MigrationUtil migrationUtil;

    @Mock
    private MasterDataUtils masterDataUtils;


    @Mock
    private IV1Service v1Service;

    @Mock
    private V1DataResponse v1DataResponse;

    @Mock
    private EmailTemplatesRequest emailTemplatesRequest;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    // ============ createTransactionHistory ============

    @Test
    void shouldSaveTransactionHistorySuccessfully() {
        carrierBookingInttraUtil.createTransactionHistory(
                "COMPLETED",
                FlowType.Outbound,
                "Test description",
                SourceSystem.INTTRA,
                123L,
                EntityTypeTransactionHistory.VGM
        );

        ArgumentCaptor<TransactionHistory> captor = ArgumentCaptor.forClass(TransactionHistory.class);
        verify(transactionHistoryDao, times(1)).save(captor.capture());

        TransactionHistory history = captor.getValue();
        assertEquals("COMPLETED", history.getActionStatusDescription());
        assertEquals(FlowType.Outbound, history.getFlowType());
        assertEquals("Test description", history.getDescription());
        assertEquals(SourceSystem.INTTRA, history.getSourceSystem());
        assertEquals(EntityTypeTransactionHistory.VGM, history.getEntityType());
        assertEquals(123L, history.getEntityId());
        assertNotNull(history.getActualDateTime());
    }

    // ============ fetchRequiredParty ============

    @Test
    void shouldReturnNull_whenPartyIsNull() {
        PartiesResponse result = carrierBookingInttraUtil.fetchRequiredParty(null);
        assertNull(result);
    }

    @Test
    void shouldMapAllFieldsCorrectly_fromPartyToPartyResponse() {
        Parties party = new Parties();
        party.setId(1L);
        party.setEntityId(123L); // entityId is now Long
        party.setEntityType("SHIPPER");
        party.setType("NOTIFY");
        party.setOrgCode("ORG001");
        party.setTenantId(10);
        party.setAddressCode("ADDR123");
        party.setOrgId("ORG-100"); // orgId is now String
        party.setAddressId("ADDR-200"); // addressId is now String

        Map<String, Object> orgData = new HashMap<>();
        orgData.put("name", "OrgName");
        party.setOrgData(orgData);

        Map<String, Object> addressData = new HashMap<>();
        addressData.put("city", "Mumbai");
        party.setAddressData(addressData);

        party.setIsAddressFreeText(true);
        party.setCountryCode("IN");

        PartiesResponse response = carrierBookingInttraUtil.fetchRequiredParty(party);

        assertNotNull(response);
        assertEquals(1L, response.getId());
        assertEquals(123L, response.getEntityId());
        assertEquals("SHIPPER", response.getEntityType());
        assertEquals("NOTIFY", response.getType());
        assertEquals("ORG001", response.getOrgCode());
        assertEquals(10, response.getTenantId());
        assertEquals("ADDR123", response.getAddressCode());
        assertEquals("ORG-100", response.getOrgId());
        assertEquals("ADDR-200", response.getAddressId());
        assertEquals(orgData, response.getOrgData());
        assertEquals(addressData, response.getAddressData());
        assertTrue(response.getIsAddressFreeText());
        assertEquals("IN", response.getCountryCode());
    }

    @Test
    void shouldSendPayloadToBridgeSuccessfully() throws RunnerException {
        // Given
        Map<String, Object> payload = new HashMap<>();
        payload.put("id", "123");
        payload.put("type", "test");

        Long id = 1L;
        String integrationCode = "INTTRA_BOOKING";
        String transactionId = "txn-123";
        String referenceId = "ref-456";
        IntegrationType integrationType = IntegrationType.BRIDGE_VGM_AMEND;
        String entityType = "VGM";

        BridgeServiceResponse mockResponse = mock(BridgeServiceResponse.class);
        when(jsonHelper.convertToJson(payload)).thenReturn("{\"id\":\"123\",\"type\":\"test\"}");
        when(mockResponse.toString()).thenReturn("Bridge response OK");

        when(bridgeServiceAdapter.bridgeApiIntegration(payload, integrationCode, transactionId, referenceId))
                .thenReturn(mockResponse);

        // When
        carrierBookingInttraUtil.sendPayloadToBridge(payload, id, integrationCode, transactionId, referenceId, integrationType, entityType);

        // Then
        verify(jsonHelper).convertToJson(payload);
        verify(bridgeServiceAdapter).bridgeApiIntegration(payload, integrationCode, transactionId, referenceId);
        verify(migrationUtil).saveErrorResponse(id, entityType, integrationType, Status.SUCCESS, "Bridge response OK");
    }

    @Test
    void shouldHandleExceptionFromBridgeApiIntegration() throws RunnerException {
        // Given
        Map<String, Object> payload = new HashMap<>();
        payload.put("id", "123");
        payload.put("type", "test");

        Long id = 1L;
        String integrationCode = "INTTRA_BOOKING";
        String transactionId = "txn-123";
        String referenceId = "ref-456";
        IntegrationType integrationType = IntegrationType.BRIDGE_VGM_SUBMIT;
        String entityType = "VGM";

        RuntimeException bridgeException = new RuntimeException("Bridge failed");
        when(jsonHelper.convertToJson(payload)).thenReturn("{\"id\":\"123\",\"type\":\"test\"}");
        when(bridgeServiceAdapter.bridgeApiIntegration(payload, integrationCode, transactionId, referenceId))
                .thenThrow(bridgeException);

        // When + Then
        RunnerException thrown = assertThrows(RunnerException.class, () ->
                carrierBookingInttraUtil.sendPayloadToBridge(payload, id, integrationCode, transactionId, referenceId, integrationType, entityType)
        );

        assertEquals("Getting error from Bridge", thrown.getMessage());
        verify(migrationUtil).saveErrorResponse(id, entityType, integrationType, Status.FAILED, "Bridge failed");
    }


    @Test
    void shouldFetchCarrierDetailsForBridgePayloadSuccessfully() {
        // Given
        SailingInformationResponse sailingInfo = mock(SailingInformationResponse.class);
        Set<String> carrierCodes = Set.of("MAEU", "HLCU");

        Map<String, EntityTransferCarrier> expectedMap = new HashMap<>();
        expectedMap.put("MAEU", new EntityTransferCarrier());
        expectedMap.put("HLCU", new EntityTransferCarrier());

        when(masterDataUtils.createInBulkCarriersRequest(
                eq(sailingInfo),
                eq(SailingInformation.class),
                anyMap(),
                eq(SailingInformation.class.getSimpleName()),
                anyMap()))
                .thenReturn(new ArrayList<>(carrierCodes));

        when(masterDataUtils.fetchInBulkCarriers(carrierCodes))
                .thenReturn(expectedMap);

        // When
        Map<String, EntityTransferCarrier> result =
                carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(sailingInfo);

        // Then
        assertEquals(2, result.size());
        assertTrue(result.containsKey("MAEU"));
        assertTrue(result.containsKey("HLCU"));

        verify(masterDataUtils).createInBulkCarriersRequest(
                eq(sailingInfo),
                eq(SailingInformation.class),
                anyMap(),
                eq(SailingInformation.class.getSimpleName()),
                anyMap());

        verify(masterDataUtils).fetchInBulkCarriers(any());
    }

    @Test
    void shouldReturnEmptyCarrierMapWhenNoCarriersFound() {
        // Given
        SailingInformationResponse sailingInfo = new SailingInformationResponse();

        when(masterDataUtils.createInBulkCarriersRequest(
                eq(sailingInfo),
                eq(SailingInformation.class),
                anyMap(),
                eq(SailingInformation.class.getSimpleName()),
                anyMap()))
                .thenReturn(Collections.emptyList());

        // When
        Map<String, EntityTransferCarrier> result =
                carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(sailingInfo);

        // Then
        assertTrue(result.isEmpty());
        verify(masterDataUtils).createInBulkCarriersRequest(
                eq(sailingInfo),
                eq(SailingInformation.class),
                anyMap(),
                eq(SailingInformation.class.getSimpleName()),
                anyMap());
        verify(masterDataUtils, never()).fetchInBulkCarriers(anySet());
    }

    @Test
    void shouldAddAllContainerTypesInSingleCallSuccessfully() {
        // Given
        CommonContainerResponse containerResponse = mock(CommonContainerResponse.class);
        List<CommonContainerResponse> responses = List.of(containerResponse);

        List<String> containerTypes = List.of("20GP", "40HC");

        Map<String, EntityTransferContainerType> expectedMap = new HashMap<>();
        expectedMap.put("20GP", new EntityTransferContainerType());
        expectedMap.put("40HC", new EntityTransferContainerType());

        when(masterDataUtils.createInBulkContainerTypeRequest(
                eq(containerResponse),
                eq(CommonContainers.class),
                anyMap(),
                eq(CommonContainers.class.getSimpleName()),
                anyMap()))
                .thenReturn(containerTypes);

        when(masterDataUtils.fetchInBulkContainerTypes(any()))
                .thenReturn(expectedMap);

        // When
        Map<String, EntityTransferContainerType> result =
                carrierBookingInttraUtil.addAllContainerTypesInSingleCall(responses);

        // Then
        assertEquals(2, result.size());
        assertTrue(result.containsKey("20GP"));
        assertTrue(result.containsKey("40HC"));

        verify(masterDataUtils).createInBulkContainerTypeRequest(
                eq(containerResponse),
                eq(CommonContainers.class),
                anyMap(),
                eq(CommonContainers.class.getSimpleName()),
                anyMap());

    }

    @Test
    void shouldReturnEmptyMapWhenContainerResponsesIsNull() {
        // When
        Map<String, EntityTransferContainerType> result =
                carrierBookingInttraUtil.addAllContainerTypesInSingleCall(null);

        // Then
        assertTrue(result.isEmpty());
        verify(masterDataUtils, never()).createInBulkContainerTypeRequest(any(), any(), anyMap(), any(), anyMap());
        verify(masterDataUtils, never()).fetchInBulkContainerTypes(anySet());
    }


    @Test
    void validateContainersIntegrationCode_EmptyList_ShouldReturn() {
        carrierBookingInttraUtil.validateContainersIntegrationCode(new ArrayList<>());
    }

    @Test
    void validateContainersIntegrationCode_AllValidIntegrationCodes_ShouldPass() {
        // Setup
        List<CommonContainerResponse> containers = Arrays.asList(
                createContainer("CONT001", "INTG001"),
                createContainer("CONT002", "INTG002"),
                createContainer("CONT003", "INTG003")
        );
        carrierBookingInttraUtil.validateContainersIntegrationCode(containers);
    }

    @Test
    void validateContainersIntegrationCode_SingleInvalidContainer_ShouldThrowException() {
        // Setup
        CommonContainerResponse container = createContainer("SINGLE_CONT", "  ");
        List<CommonContainerResponse> containers = Collections.singletonList(container);

        // Execute & Assert
        ValidationException exception = assertThrows(ValidationException.class,
                () -> carrierBookingInttraUtil.validateContainersIntegrationCode(containers));
        assertEquals("IntegrationCode is a mandatory field for INTTRA. Missing for containers: SINGLE_CONT",
                exception.getMessage());
    }

    private CommonContainerResponse createContainer(String containerNo, String integrationCode) {
        CommonContainerResponse container = new CommonContainerResponse();
        container.setContainerNo(containerNo);
        container.setIntegrationCode(integrationCode);
        return container;
    }

    @Test
    void testFetchEmailTemplate_v1DataResponseIsNull() {
        // Mocking v1Service to return null
        when(v1Service.getEmailTemplates(any(CommonV1ListRequest.class))).thenReturn(null);

        // Call the method under test
        List<EmailTemplatesRequest> result = carrierBookingInttraUtil.fetchEmailTemplate(List.of("templateCode"));

        // Verify that an empty list is returned
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFetchEmailTemplate_entitiesIsNull() {
        // Mocking v1DataResponse to return a response with null entities
        when(v1Service.getEmailTemplates(any(CommonV1ListRequest.class))).thenReturn(v1DataResponse);
        v1DataResponse = new V1DataResponse();
        v1DataResponse.setEntities(null);
        when(v1Service.getEmailTemplates(any())).thenReturn(v1DataResponse);

        // Call the method under test
        List<EmailTemplatesRequest> result = carrierBookingInttraUtil.fetchEmailTemplate(List.of("templateCode"));

        // Verify that an empty list is returned
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFetchEmailTemplate_entitiesIsNotNull() {
        // Prepare mock data
        List<EmailTemplatesRequest> mockEmailTemplates = List.of(emailTemplatesRequest);
        v1DataResponse = new V1DataResponse();
        v1DataResponse.setEntities(new CarrierBooking());
        when(v1Service.getEmailTemplates(any())).thenReturn(v1DataResponse);
        when(jsonHelper.convertValueToList(any(), eq(EmailTemplatesRequest.class))).thenReturn(mockEmailTemplates);

        // Call the method under test
        List<EmailTemplatesRequest> result = carrierBookingInttraUtil.fetchEmailTemplate(List.of("templateCode"));

        // Verify that the result is as expected
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals(emailTemplatesRequest, result.get(0));
    }

    @Test
    void testFetchUnLocationMap_success() {
        // Prepare CarrierBooking with SailingInformation
        CarrierBooking carrierBooking = new CarrierBooking();
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setPod("POD123");
        sailingInfo.setPol("POL456");
        sailingInfo.setCarrierDeliveryPlace("DEL789");
        sailingInfo.setCarrierReceiptPlace("REC321");
        carrierBooking.setSailingInformation(sailingInfo);
        carrierBooking.setBookingOffice("BOOK654");

        // Mock locations map response
        Map<String, EntityTransferUnLocations> mockResponse = new HashMap<>();
        mockResponse.put("POD123", new EntityTransferUnLocations());
        mockResponse.put("POL456", new EntityTransferUnLocations());

        when(masterDataUtils.fetchInBulkUnlocations(anySet(), eq(EntityTransferConstants.LOCATION_SERVICE_GUID)))
                .thenReturn(mockResponse);

        // Call the method under test
        Map<String, EntityTransferUnLocations> result = carrierBookingInttraUtil.fetchUnLocationMap(carrierBooking);

        // Assertions
        assertNotNull(result);
        assertEquals(2, result.size());

        // Verify interaction
        verify(masterDataUtils, times(1))
                .fetchInBulkUnlocations(anySet(), eq(EntityTransferConstants.LOCATION_SERVICE_GUID));
    }

    @Test
    void testFetchUnLocationMap_withNullValues() {
        // Prepare CarrierBooking with null values in SailingInformation
        CarrierBooking carrierBooking = new CarrierBooking();
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setPod(null);
        sailingInfo.setPol(null);
        sailingInfo.setCarrierDeliveryPlace(null);
        sailingInfo.setCarrierReceiptPlace(null);
        carrierBooking.setSailingInformation(sailingInfo);
        carrierBooking.setBookingOffice(null);

        when(masterDataUtils.fetchInBulkUnlocations(anySet(), eq(EntityTransferConstants.LOCATION_SERVICE_GUID)))
                .thenReturn(Collections.emptyMap());

        // Call the method
        Map<String, EntityTransferUnLocations> result = carrierBookingInttraUtil.fetchUnLocationMap(carrierBooking);

        // Assertions
        assertNotNull(result);
        assertTrue(result.isEmpty());

        verify(masterDataUtils, times(1))
                .fetchInBulkUnlocations(anySet(), eq(EntityTransferConstants.LOCATION_SERVICE_GUID));
    }


}
