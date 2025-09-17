package com.dpw.runner.shipment.services.utils.v3;

import com.dpw.runner.shipment.services.dao.interfaces.ICarrierBookingDao;
import com.dpw.runner.shipment.services.dto.response.carrierbooking.ContainerMisMatchWarning;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.CarrierBooking;
import com.dpw.runner.shipment.services.entity.CommonContainers;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.enums.CarrierBookingGenerationType;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

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
}
