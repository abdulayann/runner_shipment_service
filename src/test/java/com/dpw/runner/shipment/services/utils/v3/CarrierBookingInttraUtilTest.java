package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.adapters.impl.BridgeServiceAdapter;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.migration.utils.MigrationUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;

import java.util.HashMap;
import java.util.Map;

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


}
