package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassInttraResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassBridgeRequest;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.NotificationContactResponse;

import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class VerifiedGrossMassUtilTest {

    private VerifiedGrossMassUtil util;

    @Mock
    private MasterDataUtils masterDataUtils;

    @BeforeEach
    void setUp() {
        util = new VerifiedGrossMassUtil();
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testPopulateRequestorEmails_AllFieldsPresent() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails(" test1@example.com ; test2@example.com ");
        vgm.setCreateByUserEmail("creator@example.com");
        vgm.setSubmitByUserEmail("submitter@example.com");

        String result = util.populateRequestorEmails(vgm);

        assertEquals("test1@example.com;test2@example.com;creator@example.com;submitter@example.com", result);
    }

    @Test
    void testPopulateRequestorEmails_ExternalEmailsBlank() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails(" ");
        vgm.setCreateByUserEmail("creator@example.com");
        vgm.setSubmitByUserEmail("submitter@example.com");

        String result = util.populateRequestorEmails(vgm);

        assertEquals("creator@example.com;submitter@example.com", result);
    }

    @Test
    void testPopulateRequestorEmails_ExternalEmailsNull() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails(null);
        vgm.setCreateByUserEmail("creator@example.com");
        vgm.setSubmitByUserEmail("submitter@example.com");

        String result = util.populateRequestorEmails(vgm);

        assertEquals("creator@example.com;submitter@example.com", result);
    }

    @Test
    void testPopulateRequestorEmails_CreatedByEmailBlank() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails("test1@example.com");
        vgm.setCreateByUserEmail(" ");
        vgm.setSubmitByUserEmail("submitter@example.com");

        String result = util.populateRequestorEmails(vgm);

        assertEquals("test1@example.com;submitter@example.com", result);
    }

    @Test
    void testPopulateRequestorEmails_SubmitByEmailBlank() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails("test1@example.com");
        vgm.setCreateByUserEmail("creator@example.com");
        vgm.setSubmitByUserEmail("  ");

        String result = util.populateRequestorEmails(vgm);

        assertEquals("test1@example.com;creator@example.com", result);
    }

    @Test
    void testPopulateRequestorEmails_AllEmailsNullOrBlank() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails("   ");
        vgm.setCreateByUserEmail(" ");
        vgm.setSubmitByUserEmail(null);

        String result = util.populateRequestorEmails(vgm);

        assertEquals("", result);
    }

    @Test
    void testPopulateRequestorEmails_ExternalEmailsHasEmptyItems() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails("test1@example.com;; ; test2@example.com;");
        vgm.setCreateByUserEmail("creator@example.com");
        vgm.setSubmitByUserEmail("submitter@example.com");

        String result = util.populateRequestorEmails(vgm);

        assertEquals("test1@example.com;test2@example.com;creator@example.com;submitter@example.com", result);
    }

    @Test
    void testPopulateRequestorEmails_withExternalAndInternalEmails() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails("test1@example.com; test2@example.com;");
        vgm.setCreateByUserEmail("creator@example.com");
        vgm.setSubmitByUserEmail("submitter@example.com");

        String result = util.populateRequestorEmails(vgm);

        assertEquals("test1@example.com;test2@example.com;creator@example.com;submitter@example.com", result);
    }

    @Test
    void testPopulateRequestorEmails_handlesNullsGracefully() {
        VerifiedGrossMass vgm = new VerifiedGrossMass();
        vgm.setExternalEmails(null);
        vgm.setCreateByUserEmail(null);
        vgm.setSubmitByUserEmail("submit@example.com");

        String result = util.populateRequestorEmails(vgm);

        assertEquals("submit@example.com", result);
    }

    @Test
    void testMapToBridgeRequest_mapsAllFieldsCorrectly() {
        VerifiedGrossMassInttraResponse response = new VerifiedGrossMassInttraResponse();
        response.setMessageGuid(UUID.randomUUID());
        response.setMessageDateTime(LocalDateTime.now());
        response.setTenantId("testTenant");
        response.setState("ORIGINAL");
        response.setSubmitterReference("REF123");
        response.setContainer(CommonContainerResponse.builder().containerNo("CONT123").build());
        response.setRequestor(new PartiesResponse());
        response.setAuthorised(new PartiesResponse());
        response.setResponsible(new PartiesResponse());
        response.setRequestorNotificationContact(new NotificationContactResponse());
        response.setCarrierBookingNo("CB123");
        response.setCarrierScacCode("SCAC123");
        response.setCarrierDescription("Carrier Description");
        response.setCarrierNotificationContact(new NotificationContactResponse());
        response.setDelegated(true);
        response.setFileName("file.xml");

        VerifiedGrossMassBridgeRequest request = util.mapToBridgeRequest(response);

        assertEquals(response.getMessageGuid(), request.getMessageGuid());
        assertEquals(response.getCarrierScacCode(), request.getCarrierScacCode());
        assertEquals(response.getContainer().getContainerNo(), request.getContainer().getContainerNo());
        assertTrue(request.isDelegated());
        assertEquals("file.xml", request.getFileName());
    }

    @Test
    void testBuildContainerResponse_shouldBuildProperly() {
        CommonContainers container = new CommonContainers();
        container.setContainerNo("CONT-999");
        container.setVgmWeight(new BigDecimal("1200"));
        container.setVgmWeightUnit("KG");
        container.setApprovalSignature("John Doe");
        container.setApprovalDate(LocalDateTime.of(2025, 1, 1, 0, 0));
        container.setWeightDeterminationMethod(WeightDeterminationMethodType.METHOD1);
        container.setWeightDeterminationDateTime(LocalDateTime.of(2025, 1, 1, 10, 0));
        container.setGrossWeight(new BigDecimal("1500"));
        container.setGrossWeightUnit("KG");
        container.setTareWeight(new BigDecimal("300"));
        container.setTareWeightUnit("KG");
        container.setSealNumber("SEAL123");

        CommonContainerResponse response = util.buildContainerResponse(container);

        assertNotNull(response);
        assertEquals("CONT-999", response.getContainerNo());
        assertEquals(new BigDecimal("1500"), response.getGrossWeight());
        assertEquals("KG", response.getGrossWeightUnit());
        assertEquals("SEAL123", response.getSealNumber());
        assertEquals(new BigDecimal("1200"), response.getVgmWeight());
        assertEquals("KG", response.getVgmWeightUnit());
        assertEquals("John Doe", response.getApprovalSignature());
        assertEquals(WeightDeterminationMethodType.METHOD1, response.getWeightDeterminationMethod());
        assertEquals(new BigDecimal("300"), response.getTareWeight());
    }

    @Test
    void testPopulateCarrierDetails_WithNullCarrierDatav1Map() {
        // Setup
        VerifiedGrossMassInttraResponse response = mock(VerifiedGrossMassInttraResponse.class);

        // Step 1: Call the method under test
        util.populateCarrierDetails(null, response);

        // Step 2: Assertions
        verify(response, never()).setCarrierScacCode(any());
        verify(response, never()).setCarrierDescription(any());
    }

    @Test
    void testPopulateCarrierDetails_WithEmptyCarrierDatav1Map() {
        // Setup
        Map<String, EntityTransferCarrier> carrierDatav1Map = new HashMap<>();
        VerifiedGrossMassInttraResponse response = mock(VerifiedGrossMassInttraResponse.class);

        // Step 1: Call the method under test
        util.populateCarrierDetails(carrierDatav1Map, response);

        // Step 2: Assertions
        // No setter methods should be called as the map is empty
        verify(response, never()).setCarrierScacCode(any());
        verify(response, never()).setCarrierDescription(any());
    }

    @Test
    void testPopulateCarrierDetails_WithValidCarrierDatav1Map() {
        // Setup
        Map<String, EntityTransferCarrier> carrierDatav1Map = new HashMap<>();
        EntityTransferCarrier carrier = new EntityTransferCarrier();
        carrier.setItemValue("CARRIER_SCAC");
        carrier.setItemDescription("Carrier Description");
        carrier.setEmail("contact@example.com");
        carrier.setCarrierContactPerson("Contact Person");
        carrierDatav1Map.put("CARRIER_SCAC", carrier);

        VerifiedGrossMassInttraResponse response = mock(VerifiedGrossMassInttraResponse.class);

        // Step 1: Call the method under test
        util.populateCarrierDetails(carrierDatav1Map, response);

        // Step 2: Assertions
        verify(response).setCarrierScacCode("CARRIER_SCAC");
        verify(response).setCarrierDescription("Carrier Description");

        verify(response).setCarrierNotificationContact(any(NotificationContactResponse.class));
    }

    @Test
    void testPopulateCarrierDetails_WithMultipleCarriers() {
        // Setup
        Map<String, EntityTransferCarrier> carrierDatav1Map = new HashMap<>();

        // Carrier 1
        EntityTransferCarrier carrier1 = new EntityTransferCarrier();
        carrier1.setItemValue("CARRIER_SCAC1");
        carrier1.setItemDescription("Carrier 1 Description");
        carrier1.setEmail("contact1@example.com");
        carrier1.setCarrierContactPerson("Contact Person 1");
        carrierDatav1Map.put("CARRIER_SCAC1", carrier1);

        // Carrier 2
        EntityTransferCarrier carrier2 = new EntityTransferCarrier();
        carrier2.setItemValue("CARRIER_SCAC2");
        carrier2.setItemDescription("Carrier 2 Description");
        carrier2.setEmail("contact2@example.com");
        carrier2.setCarrierContactPerson("Contact Person 2");
        carrierDatav1Map.put("CARRIER_SCAC2", carrier2);

        VerifiedGrossMassInttraResponse response = mock(VerifiedGrossMassInttraResponse.class);

        util.populateCarrierDetails(carrierDatav1Map, response);
        verify(response).setCarrierScacCode("CARRIER_SCAC2");
        verify(response).setCarrierDescription("Carrier 2 Description");
    }
}
