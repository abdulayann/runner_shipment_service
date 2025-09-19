package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.dao.interfaces.ITransactionHistoryDao;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.bridgeService.BridgeServiceResponse;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.TransactionHistory;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.EntityTypeTransactionHistory;
import com.dpw.runner.shipment.services.entity.enums.FlowType;
import com.dpw.runner.shipment.services.entity.enums.SourceSystem;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class CarrierBookingInttraUtilTest {

    @InjectMocks
    private CarrierBookingInttraUtil carrierBookingInttraUtil;

    @Mock
    private ITransactionHistoryDao transactionHistoryDao;

    @Mock
    private MasterDataUtils masterDataUtils;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    // ============ isBridgeServiceResponseNotValid ============

    @Test
    void shouldReturnFalse_whenStatusCodeIs200() {
        BridgeServiceResponse response = new BridgeServiceResponse();
        Map<String, Object> params = new HashMap<>();
        params.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, "200");
        response.setExtraResponseParams(params);

        boolean result = carrierBookingInttraUtil.isBridgeServiceResponseNotValid(response);
        assertFalse(result);
    }

    @Test
    void shouldReturnFalse_whenStatusCodeIs400() {
        BridgeServiceResponse response = new BridgeServiceResponse();
        Map<String, Object> params = new HashMap<>();
        params.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, "400");
        response.setExtraResponseParams(params);

        boolean result = carrierBookingInttraUtil.isBridgeServiceResponseNotValid(response);
        assertFalse(result);
    }

    @Test
    void shouldReturnTrue_whenStatusCodeIsNot200Or400() {
        BridgeServiceResponse response = new BridgeServiceResponse();
        Map<String, Object> params = new HashMap<>();
        params.put(AwbConstants.SERVICE_HTTP_STATUS_CODE, "500");
        response.setExtraResponseParams(params);

        boolean result = carrierBookingInttraUtil.isBridgeServiceResponseNotValid(response);
        assertTrue(result);
    }

    @Test
    void shouldReturnFalse_whenStatusCodeMissing() {
        BridgeServiceResponse response = new BridgeServiceResponse();
        response.setExtraResponseParams(new HashMap<>()); // Empty map

        boolean result = carrierBookingInttraUtil.isBridgeServiceResponseNotValid(response);
        assertFalse(result);
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
    void testFetchCarrierDetailsForBridgePayload() {
        // Step 1: Setup
        VerifiedGrossMass verifiedGrossMass = new VerifiedGrossMass();
        SailingInformation sailingInformation = mock(SailingInformation.class);
        verifiedGrossMass.setSailingInformation(sailingInformation);

        List<String> carrierList = new ArrayList<>();
        carrierList.add("CARRIER1");

        // Mock the methods to return the expected data
        when(masterDataUtils.createInBulkCarriersRequest(any(), eq(SailingInformation.class), any(), any(), any()))
                .thenReturn(carrierList);
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(new HashMap<>());

        // Step 2: Call the method under test
        Map<String, EntityTransferCarrier> result = carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(verifiedGrossMass.getSailingInformation());

        // Step 3: Assertions
        assertNotNull(result);
    }

    @Test
    void testFetchCarrierDetailsForBridgePayload_WithNullSailingInformation() {

        // Step 1: Call the method under test
        Map<String, EntityTransferCarrier> result = carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(null);

        // Step 2: Assertions
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFetchCarrierDetailsForBridgePayload_WithEmptyCarrierList() {
        // Setup
        VerifiedGrossMass verifiedGrossMass = new VerifiedGrossMass();
        SailingInformation sailingInformation = mock(SailingInformation.class);
        verifiedGrossMass.setSailingInformation(sailingInformation);

        // Create an empty carrier list to simulate no carriers
        List<String> carrierList = new ArrayList<>();

        // Mock the methods to return the expected data
        when(masterDataUtils.createInBulkCarriersRequest(any(), eq(SailingInformation.class), any(), any(), any()))
                .thenReturn(carrierList);  // Empty carrier list
        when(masterDataUtils.fetchInBulkCarriers(any())).thenReturn(new HashMap<>());  // No data from the service

        // Step 1: Call the method under test
        Map<String, EntityTransferCarrier> result = carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(verifiedGrossMass.getSailingInformation());

        // Step 2: Assertions
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testFetchCarrierDetailsForBridgePayload_HandlesExceptionGracefully() {
        // Setup
        VerifiedGrossMass verifiedGrossMass = new VerifiedGrossMass();
        SailingInformation sailingInformation = mock(SailingInformation.class);
        verifiedGrossMass.setSailingInformation(sailingInformation);

        // Mock an exception in fetchInBulkCarriers
        when(masterDataUtils.createInBulkCarriersRequest(any(), eq(SailingInformation.class), any(), any(), any()))
                .thenThrow(new RuntimeException("Test Exception"));

        // Step 1: Call the method under test (expecting an exception to be handled)
        Map<String, EntityTransferCarrier> result = carrierBookingInttraUtil.fetchCarrierDetailsForBridgePayload(verifiedGrossMass.getSailingInformation());

        // Step 2: Assertions
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }
}
