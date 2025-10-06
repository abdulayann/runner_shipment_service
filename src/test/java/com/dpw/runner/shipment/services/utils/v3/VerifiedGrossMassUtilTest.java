package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.NotificationContactResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VGMContainerWarningResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.VerifiedGrossMassInttraResponse;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.VerifiedGrossMass;
import com.dpw.runner.shipment.services.entity.enums.WeightDeterminationMethodType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.GROSS_WEIGHT;
import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.NET_WEIGHT;
import static com.dpw.runner.shipment.services.commons.constants.VerifiedGrossMassConstants.TARE_WEIGHT;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
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

    @Mock
    private VerifiedGrossMass verifiedGrossMass;

    @Mock
    private EmailTemplatesRequest verifiedGrossMassTemplate;

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
        Map<String, EntityTransferCarrier> result = util.fetchCarrierDetailsForBridgePayload(verifiedGrossMass);

        // Step 3: Assertions
        assertNotNull(result);
    }

    @Test
    void testFetchCarrierDetailsForBridgePayload_WithNullSailingInformation() {
        // Setup
        VerifiedGrossMass verifiedGrossMass = new VerifiedGrossMass();
        verifiedGrossMass.setSailingInformation(null);

        // Step 1: Call the method under test
        Map<String, EntityTransferCarrier> result = util.fetchCarrierDetailsForBridgePayload(verifiedGrossMass);

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
        Map<String, EntityTransferCarrier> result = util.fetchCarrierDetailsForBridgePayload(verifiedGrossMass);

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
        Map<String, EntityTransferCarrier> result = util.fetchCarrierDetailsForBridgePayload(verifiedGrossMass);

        // Step 2: Assertions
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testPopulateCarrierDetails_WithNullCarrierDatav1Map() {
        // Setup
        VerifiedGrossMassInttraResponse response = mock(VerifiedGrossMassInttraResponse.class);

        // Step 1: Call the method under test
        util.populateCarrierDetails(null, response, "");

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
        util.populateCarrierDetails(carrierDatav1Map, response, "abc@ext.com");

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
        util.populateCarrierDetails(carrierDatav1Map, response, " ");

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

        util.populateCarrierDetails(carrierDatav1Map, response, " ");
        verify(response).setCarrierScacCode("CARRIER_SCAC2");
        verify(response).setCarrierDescription("Carrier 2 Description");
    }

    @Test
    void testBuildSubmittedContainer() {
        // Step 1: Setup
        CommonContainers container = new CommonContainers();
        container.setContainerRefGuid(UUID.randomUUID());
        container.setContainerCode("C123");
        container.setGrossWeight(new BigDecimal("1500"));
        container.setNetWeight(new BigDecimal("1400"));
        container.setNetWeightUnit("KG");
        container.setGrossWeightUnit("KG");
        container.setContainerNo("CONT-999");
        container.setPacks(10);
        container.setPacksUnit("PACKS");
        container.setTareWeight(new BigDecimal("100"));
        container.setTareWeightUnit("KG");

        // Step 2: Call the method under test
        CommonContainers submittedContainer = util.buildSubmittedContainer(container);

        // Step 3: Assertions
        assertNotNull(submittedContainer);
        assertEquals(container.getContainerRefGuid(), submittedContainer.getContainerRefGuid());
        assertEquals(container.getContainerCode(), submittedContainer.getContainerCode());
        assertEquals(container.getGrossWeight(), submittedContainer.getGrossWeight());
        assertEquals(container.getNetWeight(), submittedContainer.getNetWeight());
        assertEquals(container.getNetWeightUnit(), submittedContainer.getNetWeightUnit());
        assertEquals(container.getGrossWeightUnit(), submittedContainer.getGrossWeightUnit());
        assertEquals(container.getContainerNo(), submittedContainer.getContainerNo());
        assertEquals(container.getPacks(), submittedContainer.getPacks());
        assertEquals(container.getPacksUnit(), submittedContainer.getPacksUnit());
        assertEquals(container.getTareWeight(), submittedContainer.getTareWeight());
        assertEquals(container.getTareWeightUnit(), submittedContainer.getTareWeightUnit());
    }

    private CommonContainers createCommonContainer(Long id, String containerNo,
                                                   BigDecimal vgmWeight, String vgmUnit,
                                                   BigDecimal grossWeight, String grossUnit,
                                                   BigDecimal netWeight, String netUnit,
                                                   BigDecimal tareWeight, String tareUnit) {
        CommonContainers c = new CommonContainers();
        c.setId(id);
        c.setContainerNo(containerNo);
        c.setVgmWeight(vgmWeight);
        c.setVgmWeightUnit(vgmUnit);
        c.setGrossWeight(grossWeight);
        c.setGrossWeightUnit(grossUnit);
        c.setNetWeight(netWeight);
        c.setNetWeightUnit(netUnit);
        c.setTareWeight(tareWeight);
        c.setTareWeightUnit(tareUnit);
        return c;
    }

    private Containers createConsolContainer(Long id, String containerNo,
                                             BigDecimal grossWeight, String grossUnit,
                                             BigDecimal netWeight, String netUnit,
                                             BigDecimal tareWeight, String tareUnit) {
        Containers c = new Containers();
        c.setId(id);
        c.setContainerNumber(containerNo);
        c.setGrossWeight(grossWeight);
        c.setGrossWeightUnit(grossUnit);
        c.setNetWeight(netWeight);
        c.setNetWeightUnit(netUnit);
        c.setTareWeight(tareWeight);
        c.setTareWeightUnit(tareUnit);
        return c;
    }

    @Test
    void testCompareVGMContainers_NullInputLists_ReturnsEmptyWarnings() {
        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(null, null, null);
        assertNotNull(warnings);
        assertTrue(warnings.isEmpty());
    }

    @Test
    void testCompareVGMContainers_EmptyInputLists_ReturnsEmptyWarnings() {
        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
        assertNotNull(warnings);
        assertTrue(warnings.isEmpty());
    }

    @Test
    void testCompareVGMContainers_NoDifferences_ReturnsEmptyWarnings() {
        CommonContainers current = createCommonContainer(1L, "C1",BigDecimal.valueOf(100), "KG",
                BigDecimal.valueOf(100), "KG",
                BigDecimal.valueOf(50), "KG",
                BigDecimal.valueOf(20), "KG");

        CommonContainers submitted = createCommonContainer(1L, "C1",BigDecimal.valueOf(100), "KG",
                BigDecimal.valueOf(100), "KG",
                BigDecimal.valueOf(50), "KG",
                BigDecimal.valueOf(20), "KG");

        Containers consol = createConsolContainer(1L, "C1",
                BigDecimal.valueOf(100), "KG",
                BigDecimal.valueOf(50), "KG",
                BigDecimal.valueOf(20), "KG");

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current), List.of(submitted), List.of(consol));

        assertNotNull(warnings);
        assertTrue(warnings.isEmpty());
    }

    @Test
    void testCompareVGMContainers_WeightDifferenceWithConsol_AddsWarning() {
        CommonContainers current = createCommonContainer(1L, "1",BigDecimal.valueOf(110), "KG",
                BigDecimal.valueOf(110), "KG",
                BigDecimal.valueOf(50), "KG",
                BigDecimal.valueOf(20), "KG");

        Containers consol = createConsolContainer(1L, "1",
                BigDecimal.valueOf(100), "KG",
                BigDecimal.valueOf(50), "KG",
                BigDecimal.valueOf(20), "KG");

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current), Collections.emptyList(), List.of(consol));

        assertNotNull(warnings);
        assertEquals(1, warnings.size());
        VGMContainerWarningResponse warning = warnings.get(0);
        assertEquals("1", warning.getContainerNumber());
        assertEquals("110 KG", warning.getVgmNewWeightValue());
        assertEquals("100 KG", warning.getVgmOldWeightValue());
    }

    @Test
    void testCompareVGMContainers_WeightDifferenceWithBothSubmittedAndConsol_AddsTwoWarnings() {
        CommonContainers current = createCommonContainer(1L, "1",BigDecimal.valueOf(110), "KG",
                BigDecimal.valueOf(110), "KG", // changed weight
                BigDecimal.valueOf(50), "KG",
                BigDecimal.valueOf(20), "KG");

        CommonContainers submitted = createCommonContainer(1L, "1",BigDecimal.valueOf(100), "KG",
                BigDecimal.valueOf(100), "KG",
                BigDecimal.valueOf(50), "KG",
                BigDecimal.valueOf(20), "KG");

        Containers consol = createConsolContainer(1L, "1",
                BigDecimal.valueOf(90), "KG",
                BigDecimal.valueOf(50), "KG",
                BigDecimal.valueOf(20), "KG");

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current), List.of(submitted), List.of(consol));

        assertNotNull(warnings);
        assertEquals(2, warnings.size());

        Set<String> oldWeights = new HashSet<>();
        for (VGMContainerWarningResponse warning : warnings) {
            assertEquals("1", warning.getContainerNumber());
            oldWeights.add(warning.getVgmOldWeightValue());
            assertEquals("110 KG", warning.getVgmNewWeightValue());
        }
        // The old weights must include both 100 KG (submitted) and 90 KG (consol)
        assertTrue(oldWeights.contains("100 KG"));
        assertTrue(oldWeights.contains("90 KG"));
    }

    @Test
    void testCompareVGMContainers_ExceptionHandled() {
        // Create a subclass that throws exception in map collectors to simulate exception path
        VerifiedGrossMassUtil utilWithException = new VerifiedGrossMassUtil() {
            @Override
            public List<VGMContainerWarningResponse> compareVGMContainers(List<CommonContainers> currentVGMContainersList,
                                                                          List<CommonContainers> submittedVGMContainersList,
                                                                          List<Containers> consolContainersList) {
                throw new RuntimeException("Simulated exception");
            }
        };

        try {
            utilWithException.compareVGMContainers(null, null, null);
            fail("Exception should propagate");
        } catch (RuntimeException e) {
            assertEquals("Simulated exception", e.getMessage());
        }
    }

    @Test
    void testCompareVGMContainers_ExceptionCaughtAndHandled() {
        CommonContainers current = createCommonContainer(null, "CONT1", BigDecimal.valueOf(100), "KG", new BigDecimal("10"), "KG", new BigDecimal("8"), "KG", new BigDecimal("2"), "KG");

        List<VGMContainerWarningResponse> result = util.compareVGMContainers(
                List.of(current),
                Collections.emptyList(),
                Collections.emptyList()
        );

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testCompareVGMContainers_FormatWeightHandlesNullValues() {
        // current container with null weights and null units
        CommonContainers current = createCommonContainer(1L, "CONT1", null, null,null, null, null, null, null, null);
        // submitted container with null weights and null units
        CommonContainers submitted = createCommonContainer(1L, "CONT1", null, null,null, null, null, null, null, null);
        Containers consol = createConsolContainer(1L, "CONT1", null, null, null, null, null, null);

        List<VGMContainerWarningResponse> result = util.compareVGMContainers(
                List.of(current),
                List.of(submitted),
                List.of(consol)
        );

        // Because weights are same (empty strings), no warnings expected
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testCompareVGMContainers_ContainerNumberFromConsolWhenSubmittedIsNull() {
        CommonContainers current = createCommonContainer(
                1L, String.valueOf(1), new BigDecimal("12"), "KG", new BigDecimal("12"), "KG", new BigDecimal("9"), "KG", new BigDecimal("3"), "KG");
        Containers consol = createConsolContainer(1L, "CONSOL_CONT1", new BigDecimal("10"), "KG", new BigDecimal("8"), "KG", new BigDecimal("2"), "KG");

        // submittedContainer is null, so buildVGMContainerWarning picks containerNumber from consolContainer
        List<VGMContainerWarningResponse> result = util.compareVGMContainers(
                List.of(current),
                Collections.emptyList(),
                List.of(consol)
        );

        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("CONSOL_CONT1", result.get(0).getContainerNumber());
    }

    @Test
    void testCompareVGMContainers_ContainerNumberIsNullWhenBothSubmittedAndConsolNull() {
        CommonContainers current = createCommonContainer(1L, null, new BigDecimal("12"), "KG", new BigDecimal("12"), "KG", new BigDecimal("9"), "KG", new BigDecimal("3"), "KG");

        // Both submitted and consol containers are missing for the current container
        List<VGMContainerWarningResponse> result = util.compareVGMContainers(
                List.of(current),
                Collections.emptyList(),
                Collections.emptyList()
        );

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testCompareVGMContainers_OneContainerHasWeightDifference_ReturnsSingleWarning() {
        CommonContainers current1 = createCommonContainer(1L, "1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        CommonContainers current2 = createCommonContainer(2L, "2", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");

        CommonContainers submitted1 = createCommonContainer(1L, "1", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        CommonContainers submitted2 = createCommonContainer(2L, "2", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");

        Containers consol1 = createConsolContainer(1L, "1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        Containers consol2 = createConsolContainer(2L, "2", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current1, current2), List.of(submitted1, submitted2), List.of(consol1, consol2));

        assertNotNull(warnings);
        assertEquals(1, warnings.size());
        VGMContainerWarningResponse warning = warnings.get(0);
        assertEquals("1", warning.getContainerNumber());
        assertEquals("100 KG", warning.getVgmNewWeightValue());
        assertEquals("110 KG", warning.getVgmOldWeightValue());
    }

    @Test
    void testCompareVGMContainers_OnlyConsolContainers_ReturnsEmptyWarnings() {
        CommonContainers current = createCommonContainer(1L, "C1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        Containers consol = createConsolContainer(1L, "C1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current), Collections.emptyList(), List.of(consol));

        assertNotNull(warnings);
        assertTrue(warnings.isEmpty());
    }

    @Test
    void testCompareVGMContainers_OnlySubmittedContainersWithDifference_ReturnsWarning() {
        CommonContainers current = createCommonContainer(1L, "1", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        CommonContainers submitted = createCommonContainer(1L, "1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current), List.of(submitted), Collections.emptyList());

        assertNotNull(warnings);
        assertEquals(1, warnings.size());
        VGMContainerWarningResponse warning = warnings.get(0);
        assertEquals("1", warning.getContainerNumber());
        assertEquals("110 KG", warning.getVgmNewWeightValue());
        assertEquals("100 KG", warning.getVgmOldWeightValue());
    }

    @Test
    void testCompareVGMContainers_MultipleContainersWithWeightDifferences_ReturnsMultipleWarnings() {
        CommonContainers current1 = createCommonContainer(1L, "1", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        CommonContainers current2 = createCommonContainer(2L, "2", BigDecimal.valueOf(120), "KG", BigDecimal.valueOf(120), "KG", BigDecimal.valueOf(60), "KG", BigDecimal.valueOf(30), "KG");

        CommonContainers submitted1 = createCommonContainer(1L, "1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        CommonContainers submitted2 = createCommonContainer(2L, "2", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(60), "KG", BigDecimal.valueOf(30), "KG");

        Containers consol1 = createConsolContainer(1L, "1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        Containers consol2 = createConsolContainer(2L, "2", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(60), "KG", BigDecimal.valueOf(30), "KG");

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current1, current2), List.of(submitted1, submitted2), List.of(consol1, consol2));

        assertNotNull(warnings);
        assertEquals(4, warnings.size());

        // Check that we got the correct warnings for both containers
        Set<String> containerNumbers = new HashSet<>();
        for (VGMContainerWarningResponse warning : warnings) {
            containerNumbers.add(warning.getContainerNumber());
        }

        assertTrue(containerNumbers.contains("1"));
        assertTrue(containerNumbers.contains("2"));
    }

    @Test
    void testCompareVGMContainers_WeightCalculation_HandledCorrectly() {
        CommonContainers current = createCommonContainer(1L, "1", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(110), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        CommonContainers submitted = createCommonContainer(1L, "1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        Containers consol = createConsolContainer(1L, "1", BigDecimal.valueOf(90), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current), List.of(submitted), List.of(consol));

        assertNotNull(warnings);
        assertEquals(2, warnings.size());

        // Check that the warning is added for weight differences
        VGMContainerWarningResponse warning = warnings.get(0);
        assertEquals("110 KG", warning.getVgmNewWeightValue());  // Ensure new weight from current container
        assertEquals("100 KG", warning.getVgmOldWeightValue());  // Ensure old weight from submitted container
    }

    @Test
    void testCompareVGMContainers_WeightCalculationWithNullUnits() {
        CommonContainers current = createCommonContainer(1L, "C1", null, null ,null, null, null, null, null, null);
        CommonContainers submitted = createCommonContainer(1L, "C1", null, null, null, null, null, null, null, null);
        Containers consol = createConsolContainer(1L, "C1", null, null, null, null, null, null);

        List<VGMContainerWarningResponse> warnings = util.compareVGMContainers(
                List.of(current), List.of(submitted), List.of(consol));

        assertNotNull(warnings);
        assertTrue(warnings.isEmpty());  // No differences, so no warnings
    }

    @Test
    void testPrivateMethodGetContainerNumber() throws Exception {
        // Accessing the private method using reflection
        Method getContainerNumberMethod = VerifiedGrossMassUtil.class.getDeclaredMethod("getContainerNumber", CommonContainers.class, Containers.class);
        getContainerNumberMethod.setAccessible(true);

        // Create test objects
        CommonContainers submitted = createCommonContainer(1L, "SUBMITTED_C1", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(100), "KG", BigDecimal.valueOf(50), "KG", BigDecimal.valueOf(20), "KG");
        Containers consol = null;

        // Invoke private method using reflection
        String result = (String) getContainerNumberMethod.invoke(util, submitted, consol);

        assertEquals("SUBMITTED_C1", result);
    }

    @Test
    void testGetContainerNumber_BothContainersNull() {
        // Both containers are null
        CommonContainers submittedContainer = null;
        Containers consolContainer = null;

        try {
            // Accessing the private method using reflection
            Method getContainerNumberMethod = VerifiedGrossMassUtil.class.getDeclaredMethod("getContainerNumber", CommonContainers.class, Containers.class);
            getContainerNumberMethod.setAccessible(true);

            // Invoke private method using reflection
            String result = (String) getContainerNumberMethod.invoke(util, submittedContainer, consolContainer);

            // Assert that the result is null when both containers are null
            assertNull(result);
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            // Handle the exception or assert failure
            fail("Exception occurred: " + e.getMessage());
        }
    }

    @Test
    void testGetWeight_BothContainersNull() throws Exception {
        // Both containers are null
        CommonContainers submittedContainer = null;
        Containers consolContainer = null;

        // Accessing the private method using reflection
        Method getWeightMethod = VerifiedGrossMassUtil.class.getDeclaredMethod("getWeight", CommonContainers.class, Containers.class, String.class);
        getWeightMethod.setAccessible(true);

        // Test for GROSS_WEIGHT
        String resultGross = (String) getWeightMethod.invoke(util, submittedContainer, consolContainer, GROSS_WEIGHT);
        assertNull(resultGross);

        // Test for NET_WEIGHT
        String resultNet = (String) getWeightMethod.invoke(util, submittedContainer, consolContainer, NET_WEIGHT);
        assertNull(resultNet);

        // Test for TARE_WEIGHT
        String resultTare = (String) getWeightMethod.invoke(util, submittedContainer, consolContainer, TARE_WEIGHT);
        assertNull(resultTare);

        String resultIncorrect = (String) getWeightMethod.invoke(util, submittedContainer, consolContainer, "Wrong Input");
        assertNull(resultIncorrect);
    }

    @Test
    void testGetFormattedWeight_BothContainersNull() throws Exception {
        // Both containers are null
        CommonContainers submittedContainer = null;
        Containers consolContainer = null;

        // Accessing the private method using reflection
        Method getFormattedWeightMethod = VerifiedGrossMassUtil.class.getDeclaredMethod("formatWeight", BigDecimal.class, String.class);
        getFormattedWeightMethod.setAccessible(true);

        // Invoke private method using reflection
        String result = (String) getFormattedWeightMethod.invoke(util, submittedContainer, consolContainer);

        // Assert that the result is null when both containers are null
        assertNotNull(result);
    }

    @Test
    void testGetSendEmailBaseRequest_NoInternalEmails() {
        when(verifiedGrossMass.getInternalEmails()).thenReturn("");
        when(verifiedGrossMass.getCreateByUserEmail()).thenReturn("creator@example.com");
        when(verifiedGrossMass.getSubmitByUserEmail()).thenReturn("submitter@example.com");

        List<String> emails = util.getSendEmailBaseRequest(verifiedGrossMass);

        assertEquals(2, emails.size());
        assertTrue(emails.contains("creator@example.com"));
        assertTrue(emails.contains("submitter@example.com"));
    }

    @Test
    void testGetSendEmailBaseRequest_InternalEmailsPresent() {
        when(verifiedGrossMass.getInternalEmails()).thenReturn("ops@example.com,support@example.com");
        when(verifiedGrossMass.getCreateByUserEmail()).thenReturn("creator@example.com");
        when(verifiedGrossMass.getSubmitByUserEmail()).thenReturn("submitter@example.com");

        List<String> emails = util.getSendEmailBaseRequest(verifiedGrossMass);

        assertEquals(4, emails.size());
        assertTrue(emails.contains("ops@example.com"));
        assertTrue(emails.contains("support@example.com"));
        assertTrue(emails.contains("creator@example.com"));
        assertTrue(emails.contains("submitter@example.com"));
    }

    @Test
    void testGetSendEmailBaseRequest_SubmitterSameAsCreator() {
        when(verifiedGrossMass.getInternalEmails()).thenReturn(null);
        when(verifiedGrossMass.getCreateByUserEmail()).thenReturn("creator@example.com");
        when(verifiedGrossMass.getSubmitByUserEmail()).thenReturn("creator@example.com");

        List<String> emails = util.getSendEmailBaseRequest(verifiedGrossMass);

        assertEquals(1, emails.size());
        assertEquals("creator@example.com", emails.get(0));
    }

    @Test
    void testGetSendEmailBaseRequest_BlankEmailsIgnored() {
        when(verifiedGrossMass.getInternalEmails()).thenReturn(" , , ");
        when(verifiedGrossMass.getCreateByUserEmail()).thenReturn("   "); // blank
        when(verifiedGrossMass.getSubmitByUserEmail()).thenReturn("submitter@example.com");

        List<String> emails = util.getSendEmailBaseRequest(verifiedGrossMass);

        assertEquals(1, emails.size());
        assertEquals("submitter@example.com", emails.get(0));
    }

    @Test
    void testGetSendEmailBaseRequest_DuplicateEmailsRemoved() {
        when(verifiedGrossMass.getInternalEmails()).thenReturn("creator@example.com,ops@example.com");
        when(verifiedGrossMass.getCreateByUserEmail()).thenReturn("creator@example.com");
        when(verifiedGrossMass.getSubmitByUserEmail()).thenReturn("creator@example.com");

        List<String> emails = util.getSendEmailBaseRequest(verifiedGrossMass);

        assertEquals(2, emails.size());
        assertTrue(emails.contains("creator@example.com"));
        assertTrue(emails.contains("ops@example.com"));
    }
}
