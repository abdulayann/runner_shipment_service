package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassInttraResponse;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.VerifiedGrossMassBridgeRequest;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.NotificationContactResponse;

import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

class VerifiedGrossMassUtilTest {

    private VerifiedGrossMassUtil util;

    @BeforeEach
    void setUp() {
        util = new VerifiedGrossMassUtil();
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
}
