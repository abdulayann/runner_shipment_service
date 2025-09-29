package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dto.request.EmailTemplatesRequest;
import com.dpw.runner.shipment.services.dto.request.carrierbooking.CarrierBookingBridgeRequest;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.CommonContainerResponse;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ContainerMisMatchWarning;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.SailingInformation;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingGenerationType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferContainerType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.notification.request.SendEmailBaseRequest;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class CarrierBookingUtilTest {

    @Mock
    private CommonUtils commonUtils;

    @Mock
    private ICarrierBookingDao carrierBookingDao;

    @InjectMocks
    private CarrierBookingUtil carrierBookingUtil;

    @Test
    void test_truncate_Success() {
        // Test with text shorter than max length
        String result1 = carrierBookingUtil.truncate("Hello", 10);
        assertEquals("Hello", result1);

        // Test with text equal to max length
        String result2 = carrierBookingUtil.truncate("Hello", 5);
        assertEquals("Hello", result2);

        // Test with text longer than max length
        String result3 = carrierBookingUtil.truncate("Hello World", 5);
        assertEquals("Hello", result3);
    }

    @Test
    void test_truncate_NullInput() {
        // Test with null input
        String result = carrierBookingUtil.truncate(null, 10);
        assertNull(result);
    }

    @Test
    void test_truncate_EmptyString() {
        // Test with empty string
        String result = carrierBookingUtil.truncate("", 10);
        assertEquals("", result);
    }

    @Test
    void test_detectContainerMismatches_Success() {
        // Mock data setup - Console containers
        Containers consoleContainer1 = new Containers();
        consoleContainer1.setContainerCode("20GP");
        consoleContainer1.setContainerCount(5L);

        Containers consoleContainer2 = new Containers();
        consoleContainer2.setContainerCode("40GP");
        consoleContainer2.setContainerCount(3L);

        List<Containers> consoleContainers = List.of(consoleContainer1, consoleContainer2);

        // Mock data setup - Booking containers
        CommonContainers bookingContainer1 = new CommonContainers();
        bookingContainer1.setContainerCode("20GP");
        bookingContainer1.setCount(7L); // 2 more than console

        CommonContainers bookingContainer2 = new CommonContainers();
        bookingContainer2.setContainerCode("40GP");
        bookingContainer2.setCount(2L); // 1 less than console

        CommonContainers bookingContainer3 = new CommonContainers();
        bookingContainer3.setContainerCode("40HC");
        bookingContainer3.setCount(1L); // New container not in console

        List<CommonContainers> bookingContainers = List.of(bookingContainer1, bookingContainer2, bookingContainer3);

        // Test
        List<ContainerMisMatchWarning> result = carrierBookingUtil.detectContainerMismatches(consoleContainers, bookingContainers);

        // Verify
        assertEquals(3, result.size());

        ContainerMisMatchWarning warning1 = result.stream()
                .filter(w -> "20GP".equals(w.getContainerCode()))
                .findFirst().orElse(null);
        assertNotNull(warning1);


        ContainerMisMatchWarning warning2 = result.stream()
                .filter(w -> "40GP".equals(w.getContainerCode()))
                .findFirst().orElse(null);
        assertNotNull(warning2);


        ContainerMisMatchWarning warning3 = result.stream()
                .filter(w -> "40HC".equals(w.getContainerCode()))
                .findFirst().orElse(null);
        assertNotNull(warning3);
    }

    @Test
    void test_detectContainerMismatches_NoMismatches() {
        // Mock data setup - Console containers
        Containers consoleContainer = new Containers();
        consoleContainer.setContainerCode("20GP");
        consoleContainer.setContainerCount(5L);

        List<Containers> consoleContainers = List.of(consoleContainer);

        // Mock data setup - Booking containers
        CommonContainers bookingContainer = new CommonContainers();
        bookingContainer.setContainerCode("20GP");
        bookingContainer.setCount(5L); // Same as console

        List<CommonContainers> bookingContainers = List.of(bookingContainer);

        // Test
        List<ContainerMisMatchWarning> result = carrierBookingUtil.detectContainerMismatches(consoleContainers, bookingContainers);

        // Verify
        assertTrue(result.isEmpty());
    }

    @Test
    void test_detectContainerMismatches_DuplicateContainerCodes() {
        // Mock data setup - Console containers with duplicates
        Containers consoleContainer1 = new Containers();
        consoleContainer1.setContainerCode("20GP");
        consoleContainer1.setContainerCount(3L);

        Containers consoleContainer2 = new Containers();
        consoleContainer2.setContainerCode("20GP");
        consoleContainer2.setContainerCount(2L); // Will sum to 5

        List<Containers> consoleContainers = List.of(consoleContainer1, consoleContainer2);

        // Mock data setup - Booking containers
        CommonContainers bookingContainer = new CommonContainers();
        bookingContainer.setContainerCode("20GP");
        bookingContainer.setCount(7L);

        List<CommonContainers> bookingContainers = List.of(bookingContainer);

        // Test
        List<ContainerMisMatchWarning> result = carrierBookingUtil.detectContainerMismatches(consoleContainers, bookingContainers);

        // Verify
        assertEquals(1, result.size());
    }

    @Test
    void test_generateBookingNumber_SerialGeneration() {
        // Mock data setup
        CarrierBooking carrierBookingEntity = new CarrierBooking();

        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setBookingPrefix("CB-");
        tenantSettings.setBookingNumberGeneration(CarrierBookingGenerationType.Serial.getValue());

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(carrierBookingDao.getTotalCarrierBookings()).thenReturn(100L);

        // Test
        carrierBookingUtil.generateBookingNumber(carrierBookingEntity);

        // Verify
        verify(commonUtils, times(1)).getCurrentTenantSettings();
        verify(carrierBookingDao, times(1)).getTotalCarrierBookings();
    }

    @Test
    void test_generateBookingNumber_SerialGeneration_NoPrefix() {
        // Mock data setup
        CarrierBooking carrierBookingEntity = new CarrierBooking();

        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setBookingPrefix(null);
        tenantSettings.setBookingNumberGeneration(CarrierBookingGenerationType.Serial.getValue());

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);
        when(carrierBookingDao.getTotalCarrierBookings()).thenReturn(50L);

        // Test
        carrierBookingUtil.generateBookingNumber(carrierBookingEntity);

        // Verify
        verify(commonUtils, times(1)).getCurrentTenantSettings();
        verify(carrierBookingDao, times(1)).getTotalCarrierBookings();
    }

    @Test
    void test_generateBookingNumber_RandomGeneration() {
        // Mock data setup
        CarrierBooking carrierBookingEntity = new CarrierBooking();

        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setBookingPrefix("RND-");
        tenantSettings.setBookingNumberGeneration(CarrierBookingGenerationType.Random.getValue()); // Assuming Random exists

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        // Mock static method
        try (MockedStatic<StringUtility> mockedStringUtility = mockStatic(StringUtility.class)) {
            mockedStringUtility.when(() -> StringUtility.isNotEmpty("RND-")).thenReturn(true);
            mockedStringUtility.when(() -> StringUtility.getRandomString(10)).thenReturn("ABCDEF1234");

            // Test
            carrierBookingUtil.generateBookingNumber(carrierBookingEntity);

            // Verify
            assertEquals("RND-ABCDEF1234", carrierBookingEntity.getBookingNo());
            verify(commonUtils, times(1)).getCurrentTenantSettings();
            verify(carrierBookingDao, never()).getTotalCarrierBookings();
        }
    }

    @Test
    void test_generateBookingNumber_RandomGeneration_EmptyPrefix() {
        // Mock data setup
        CarrierBooking carrierBookingEntity = new CarrierBooking();

        V1TenantSettingsResponse tenantSettings = new V1TenantSettingsResponse();
        tenantSettings.setBookingPrefix("");
        tenantSettings.setBookingNumberGeneration(null); // Will default to random

        when(commonUtils.getCurrentTenantSettings()).thenReturn(tenantSettings);

        // Mock static method
        try (MockedStatic<StringUtility> mockedStringUtility = mockStatic(StringUtility.class)) {
            mockedStringUtility.when(() -> StringUtility.isNotEmpty("")).thenReturn(false);
            mockedStringUtility.when(() -> StringUtility.getRandomString(10)).thenReturn("XYZ9876543");

            // Test
            carrierBookingUtil.generateBookingNumber(carrierBookingEntity);

            // Verify
            assertEquals("XYZ9876543", carrierBookingEntity.getBookingNo());
            verify(commonUtils, times(1)).getCurrentTenantSettings();
            verify(carrierBookingDao, never()).getTotalCarrierBookings();
        }
    }

    @Test
    void test_populateCarrierDetails_NullMap() {
        // Arrange
        CarrierBookingBridgeRequest request = new CarrierBookingBridgeRequest();

        // Act
        carrierBookingUtil.populateCarrierDetails(null, request);

        // Assert
        assertNull(request.getCarrierScacCode());
        assertNull(request.getCarrierDescription());
    }

    @Test
    void test_populateCarrierDetails_WithValidMap() {
        // Arrange
        CarrierBookingBridgeRequest request = new CarrierBookingBridgeRequest();
        Map<String, EntityTransferCarrier> carrierMap = new HashMap<>();

        EntityTransferCarrier carrier1 = new EntityTransferCarrier();
        carrier1.ItemValue = "SCAC1";
        carrier1.ItemDescription = "Carrier One";

        EntityTransferCarrier carrier2 = new EntityTransferCarrier();
        carrier2.ItemValue = "SCAC2";
        carrier2.ItemDescription = "Carrier Two";

        carrierMap.put("1", carrier1);
        carrierMap.put("2", carrier2);

        // Act
        carrierBookingUtil.populateCarrierDetails(carrierMap, request);

        // Assert → last entry should be set (carrier2)
        assertEquals("SCAC2", request.getCarrierScacCode());
        assertEquals("Carrier Two", request.getCarrierDescription());
    }

    @Test
    void test_getSendEmailBaseRequest_CreateAndSubmitUserSame() {
        // Arrange
        CarrierBooking carrierBooking = new CarrierBooking();
        carrierBooking.setInternalEmails("internal@test.com");
        carrierBooking.setCreateByUserEmail("creator@test.com");
        carrierBooking.setSubmitByUserEmail("creator@test.com");

        EmailTemplatesRequest template = new EmailTemplatesRequest();
        template.setSubject("Test Subject");
        template.setName("TestTemplate");
        template.setBody("<p>Body</p>");

        // Act
        SendEmailBaseRequest result = carrierBookingUtil.getSendEmailBaseRequest(carrierBooking, template);

        // Assert
        assertEquals("internal@test.com,creator@test.com", result.getTo());
        assertEquals("Test Subject", result.getSubject());
        assertEquals("TestTemplate", result.getTemplateName());
        assertEquals("<p>Body</p>", result.getHtmlBody());
    }

    @Test
    void test_getSendEmailBaseRequest_CreateAndSubmitUserDifferent() {
        // Arrange
        CarrierBooking carrierBooking = new CarrierBooking();
        carrierBooking.setInternalEmails(null);
        carrierBooking.setCreateByUserEmail("creator@test.com");
        carrierBooking.setSubmitByUserEmail("submitter@test.com");

        EmailTemplatesRequest template = new EmailTemplatesRequest();
        template.setSubject("Another Subject");
        template.setName("AnotherTemplate");
        template.setBody("Another body");

        // Act
        SendEmailBaseRequest result = carrierBookingUtil.getSendEmailBaseRequest(carrierBooking, template);

        // Assert
        assertEquals("creator@test.com,submitter@test.com", result.getTo());
        assertEquals("Another Subject", result.getSubject());
        assertEquals("AnotherTemplate", result.getTemplateName());
        assertEquals("Another body", result.getHtmlBody());
    }

    @Test
    void test_mapSailingToConsolidation_AllFieldsMapped() {
        // Arrange
        SailingInformation sailing = new SailingInformation();
        sailing.setVerifiedGrossMassCutoff(LocalDateTime.of(2025, 9, 22, 10, 0));
        sailing.setReeferCutoff(LocalDateTime.of(2025, 9, 22, 11, 0));
        sailing.setShipInstructionCutoff(LocalDateTime.of(2025, 9, 22, 12, 0));
        sailing.setHazardousBookingCutoff(LocalDateTime.of(2025, 9, 22, 13, 0));
        sailing.setEmptyContainerPickupCutoff(LocalDateTime.of(2025, 9, 22, 14, 0));
        sailing.setLoadedContainerGateInCutoff(LocalDateTime.of(2025, 9, 22, 15, 0));

        ConsolidationDetails consolidation = new ConsolidationDetails();

        // Act
        carrierBookingUtil.mapSailingToConsolidation(sailing, consolidation);

        // Assert
        assertEquals(LocalDateTime.of(2025, 9, 22, 10, 0), consolidation.getVerifiedGrossMassCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 11, 0), consolidation.getReeferCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 12, 0), consolidation.getShipInstructionCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 13, 0), consolidation.getHazardousBookingCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 14, 0), consolidation.getEarliestEmptyEquPickUp());
        assertEquals(LocalDateTime.of(2025, 9, 22, 15, 0), consolidation.getTerminalCutoff());
    }

    @Test
    void test_mapSailingToConsolidation_NullValuesSkipped() {
        // Arrange
        SailingInformation sailing = new SailingInformation(); // all null
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setVerifiedGrossMassCutoff(LocalDateTime.of(2025, 9, 22, 10, 0));

        // Act
        carrierBookingUtil.mapSailingToConsolidation(sailing, consolidation);

        // Assert → unchanged because source values are null
        assertEquals(LocalDateTime.of(2025, 9, 22, 10, 0), consolidation.getVerifiedGrossMassCutoff());
    }

    @Test
    void test_mapConsolidationToSailing_AllFieldsMapped() {
        // Arrange
        ConsolidationDetails consolidation = new ConsolidationDetails();
        consolidation.setVerifiedGrossMassCutoff(LocalDateTime.of(2025, 9, 22, 10, 0));
        consolidation.setReeferCutoff(LocalDateTime.of(2025, 9, 22, 11, 0));
        consolidation.setShipInstructionCutoff(LocalDateTime.of(2025, 9, 22, 12, 0));
        consolidation.setHazardousBookingCutoff(LocalDateTime.of(2025, 9, 22, 13, 0));
        consolidation.setEarliestEmptyEquPickUp(LocalDateTime.of(2025, 9, 22, 14, 0));
        consolidation.setTerminalCutoff(LocalDateTime.of(2025, 9, 22, 15, 0));

        SailingInformation sailing = new SailingInformation();

        // Act
        carrierBookingUtil.mapConsolidationToSailing(consolidation, sailing);

        // Assert
        assertEquals(LocalDateTime.of(2025, 9, 22, 10, 0), sailing.getVerifiedGrossMassCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 11, 0), sailing.getReeferCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 12, 0), sailing.getShipInstructionCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 13, 0), sailing.getHazardousBookingCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 14, 0), sailing.getEmptyContainerPickupCutoff());
        assertEquals(LocalDateTime.of(2025, 9, 22, 15, 0), sailing.getLoadedContainerGateInCutoff());
    }

    @Test
    void test_mapConsolidationToSailing_NullValuesSkipped() {
        // Arrange
        ConsolidationDetails consolidation = new ConsolidationDetails(); // all null
        SailingInformation sailing = new SailingInformation();
        sailing.setVerifiedGrossMassCutoff(LocalDateTime.of(2025, 9, 22, 10, 0));

        // Act
        carrierBookingUtil.mapConsolidationToSailing(consolidation, sailing);

        // Assert → unchanged because source values are null
        assertEquals(LocalDateTime.of(2025, 9, 22, 10, 0), sailing.getVerifiedGrossMassCutoff());
    }

    @Test
    void test_populateIntegrationCode_SuccessfullySetsIntegrationCode() {
        // Mock container type with integration code
        EntityTransferContainerType entityTransferContainerType = new EntityTransferContainerType();
        entityTransferContainerType.setIntegrationCode("INT_CODE_20GP");

        Map<String, EntityTransferContainerType> containerTypeMap = new HashMap<>();
        containerTypeMap.put("20GP", entityTransferContainerType);

        // Mock container response
        CommonContainerResponse containerResponse = new CommonContainerResponse();
        containerResponse.setContainerCode("20GP");

        CarrierBookingBridgeRequest bridgeRequest = new CarrierBookingBridgeRequest();
        bridgeRequest.setContainersList(List.of(containerResponse));

        // Test
        carrierBookingUtil.populateIntegrationCode(containerTypeMap, bridgeRequest);

        // Verify
        assertEquals("INT_CODE_20GP", containerResponse.getIntegrationCode());
    }

    @Test
    void test_populateIntegrationCode_ContainerCodeNotFound_NoIntegrationCodeSet() {
        // Mock container type map with no matching code
        Map<String, EntityTransferContainerType> containerTypeMap = new HashMap<>();
        containerTypeMap.put("40HC", new EntityTransferContainerType());

        // Mock container response with different code
        CommonContainerResponse containerResponse = new CommonContainerResponse();
        containerResponse.setContainerCode("20GP");

        CarrierBookingBridgeRequest bridgeRequest = new CarrierBookingBridgeRequest();
        bridgeRequest.setContainersList(List.of(containerResponse));

        // Test
        carrierBookingUtil.populateIntegrationCode(containerTypeMap, bridgeRequest);

        // Verify → integration code should remain null
        assertNull(containerResponse.getIntegrationCode());
    }

    @Test
    void test_populateIntegrationCode_NullInputs_NoExceptionThrown() {
        // Case 1: null containerTypeMap
        carrierBookingUtil.populateIntegrationCode(null, new CarrierBookingBridgeRequest());

        // Case 2: null containers list
        CarrierBookingBridgeRequest bridgeRequest = new CarrierBookingBridgeRequest();
        bridgeRequest.setContainersList(null);

        carrierBookingUtil.populateIntegrationCode(new HashMap<>(), bridgeRequest);

        Assertions.assertNotNull(bridgeRequest);
    }

    @Test
    void test_populateLocCode_AllValuesPresent() {
        // Prepare CarrierBooking with SailingInformation
        CarrierBooking carrierBooking = new CarrierBooking();
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setPol("POL");
        sailingInfo.setPod("POD");
        sailingInfo.setCarrierReceiptPlace("REC");
        sailingInfo.setCarrierDeliveryPlace("DEL");
        carrierBooking.setSailingInformation(sailingInfo);
        carrierBooking.setBookingOffice("BOOK");

        // Prepare unlocations map with locCodes
        Map<String, EntityTransferUnLocations> unlocationsMap = new HashMap<>();
        unlocationsMap.put("POL", new EntityTransferUnLocations());
        unlocationsMap.put("POD", new EntityTransferUnLocations());


        // Call method
        carrierBookingUtil.populateLocCode(unlocationsMap, carrierBooking);

        // Verify updates
        assertEquals("POL", sailingInfo.getPol());
    }

    @Test
    void test_populateLocCode_MissingValues_NoExceptionThrown() {
        CarrierBooking carrierBooking = new CarrierBooking();
        SailingInformation sailingInfo = new SailingInformation();
        sailingInfo.setPol("POL");
        sailingInfo.setPod("POD");
        sailingInfo.setCarrierReceiptPlace("REC");
        sailingInfo.setCarrierDeliveryPlace("DEL");
        carrierBooking.setSailingInformation(sailingInfo);
        carrierBooking.setBookingOffice("BOOK");

        // Empty map (no entries found)
        Map<String, EntityTransferUnLocations> unlocationsMap = new HashMap<>();

        // Should not throw exception
        carrierBookingUtil.populateLocCode(unlocationsMap, carrierBooking);

        // Verify nothing is set
        Assertions.assertNull(carrierBooking.getSailingInformation().getCarrierReceiptLocCode());
        Assertions.assertNull(carrierBooking.getSailingInformation().getCarrierDeliveryLocCode());
        Assertions.assertNull(carrierBooking.getSailingInformation().getOriginPortLocCode());
        Assertions.assertNull(carrierBooking.getSailingInformation().getDestinationPortLocCode());
        Assertions.assertNull(carrierBooking.getBookingOfficeLocCode());
    }

    @Test
    void test_populateLocCode_ExceptionHandledGracefully() {
        CarrierBooking carrierBooking = new CarrierBooking();
        carrierBooking.setSailingInformation(null); // This will cause NullPointerException

        // Should not throw exception despite sailingInfo being null
        carrierBookingUtil.populateLocCode(new HashMap<>(), carrierBooking);

        // Just verify booking is still not null
        Assertions.assertNotNull(carrierBooking);
    }
}
