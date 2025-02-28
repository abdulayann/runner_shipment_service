package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dao.interfaces.IProductSequenceConfigDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ProductSequenceConfig;
import com.dpw.runner.shipment.services.entity.enums.GenerationType;
import com.dpw.runner.shipment.services.entity.enums.ProductProcessTypes;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.syncing.interfaces.IShipmentSettingsSync;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class GetNextNumberHelperTest {


    @Mock
    private IProductSequenceConfigDao productSequenceConfigDao;
    @Mock
    private IShipmentSettingsSync shipmentSettingsSync;
    @Mock
    private V1AuthHelper v1AuthHelper;

    @InjectMocks
    private GetNextNumberHelper getNextNumberHelper;


    @BeforeEach
    void init() {
        UsersDto mockUser = new UsersDto();
        mockUser.setCode("CODE");
        UserContext.setUser(mockUser);
    }



    @Test
    void generateCustomSequenceThrowsExceptionWhenRegexIsEmpty() {
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        String regexPattern = "";
        int tenantId = 1;
        boolean updateCounter = true;
        UsersDto user = UserContext.getUser();
        boolean updateBranchCode = false;

        assertThrows(RunnerException.class, () -> getNextNumberHelper.generateCustomSequence(
            sequenceSettings, regexPattern, tenantId, updateCounter, user, updateBranchCode
        ));
    }


    @Test
    void generateCustomSequenceRegexGenerationThrowsExceptionOnMisconfiguredRegex() {
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        sequenceSettings.setSerialCounter(0);
        sequenceSettings.setGenerationType(GenerationType.REGEX);
        String regexPattern = "{unspoortedString;seq}{TransportMode;1}{Date;yyMM}{seq;4}";
        int tenantId = 1;
        boolean updateCounter = true;
        UsersDto user = UserContext.getUser();
        boolean updateBranchCode = false;

        assertThrows(ValidationException.class, () -> getNextNumberHelper.generateCustomSequence(
            sequenceSettings, regexPattern, tenantId, updateCounter, user, updateBranchCode
        ));
    }

    @Test
    void generateCustomSequenceGenerationTypeRegex() {
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        sequenceSettings.setSerialCounter(0);
        sequenceSettings.setGenerationType(GenerationType.REGEX);
        String regexPattern = "{branch}{cc}{Month;1}{seq;4}";
        int tenantId = 1;
        boolean updateCounter = true;
        UsersDto user = UserContext.getUser();
        boolean updateBranchCode = true;

        try {
            var spyBean = Mockito.spy(getNextNumberHelper);
            doReturn("").when(spyBean).GetNextRegexSequenceNumber(any(), any());
            var res = spyBean.generateCustomSequence(
                sequenceSettings, regexPattern, tenantId, updateCounter, user, updateBranchCode
            );
            assertNotNull(res);
            verify(productSequenceConfigDao, times(1)).save(any());
            verify(shipmentSettingsSync, times(1)).syncProductSequence(any(), any());
        }
        catch(Exception e) {
            fail(e);
        }
    }

    @Test
    void generateCustomSequenceGenerationTypeRandom() {
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        sequenceSettings.setSerialCounter(0);
        sequenceSettings.setGenerationType(GenerationType.RANDOM);
        String regexPattern = "{TransportMode;1}{Date;yyMM}{seq;4}";
        int tenantId = 1;
        boolean updateCounter = true;
        UsersDto user = UserContext.getUser();
        boolean updateBranchCode = false;

        try {
            var res = getNextNumberHelper.generateCustomSequence(
                sequenceSettings, regexPattern, tenantId, updateCounter, user, updateBranchCode
            );
            assertNotNull(res);
            verify(productSequenceConfigDao, times(1)).save(any());
            verify(shipmentSettingsSync, times(1)).syncProductSequence(any(), any());
        }
        catch(Exception e) {
            fail(e);
        }
    }

    @Test
    void generateCustomSequenceGenerationTypeSerial() {
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        sequenceSettings.setSerialCounter(0);
        sequenceSettings.setGenerationType(GenerationType.SERIAL);
        String regexPattern = "{TransportMode;1}{Date;yyMM}{seq;4}";
        int tenantId = 1;
        boolean updateCounter = true;
        UsersDto user = UserContext.getUser();
        boolean updateBranchCode = false;

        try {
            var res = getNextNumberHelper.generateCustomSequence(
                sequenceSettings, regexPattern, tenantId, updateCounter, user, updateBranchCode
            );
            assertNotNull(res);
            verify(productSequenceConfigDao, times(1)).save(any());
            verify(shipmentSettingsSync, times(1)).syncProductSequence(any(), any());
        }
        catch(Exception e) {
            fail(e);
        }
    }

    @Test
    void generateCustomSequenceThrowsExceptionWhenResultExceeds50Length() {
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        sequenceSettings.setSerialCounter(0);
        sequenceSettings.setGenerationType(GenerationType.SERIAL);
        String prefix = StringUtility.getRandomString(50);
        String regexPattern = prefix + "{TransportMode;1}{Date;yyMM}{seq;4}";
        int tenantId = 1;
        boolean updateCounter = true;
        UsersDto user = UserContext.getUser();
        boolean updateBranchCode = false;

        assertThrows(ValidationException.class, () -> getNextNumberHelper.generateCustomSequence(
                sequenceSettings, regexPattern, tenantId, updateCounter, user, updateBranchCode
            ));
    }

    @Test
    void generateCustomSequenceGenerationTypeSerialShipmentSettingSyncFails() {
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        sequenceSettings.setSerialCounter(0);
        sequenceSettings.setGenerationType(GenerationType.SERIAL);
        String regexPattern = "{TransportMode;1}{Date;yyMM}{seq;4}";
        int tenantId = 1;
        boolean updateCounter = true;
        UsersDto user = UserContext.getUser();
        boolean updateBranchCode = false;


        try {
            when(shipmentSettingsSync.syncProductSequence(any(),any())).thenThrow(new RuntimeException());
            var res = getNextNumberHelper.generateCustomSequence(
                sequenceSettings, regexPattern, tenantId, updateCounter, user, updateBranchCode
            );
            assertNotNull(res);
            verify(productSequenceConfigDao, times(1)).save(any());
            verify(shipmentSettingsSync, times(1)).syncProductSequence(any(), any());
        }
        catch(Exception e) {
            fail(e);
        }
    }

    @Test
    void GetNextRegexSequenceNumberThrowsExceptionForNullTimeZoneId() {
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        String resetFreq = "daily";

        assertThrows(RunnerException.class,  () ->
            getNextNumberHelper.GetNextRegexSequenceNumber(sequenceSettings, resetFreq));
    }

    @Test
    void GetNextRegexSequenceNumberWithSequenceStartTimeWithOlderDate() {
        var user = UserContext.getUser();
        user.setTimeZoneId("IND");

        LocalDateTime olderDate = LocalDateTime.now().minusYears(1L);
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        sequenceSettings.setSequenceStartTime(olderDate);
        String resetFreq = "daily";


        try {
            var nextSeq = getNextNumberHelper.GetNextRegexSequenceNumber(sequenceSettings, resetFreq);
            assertNotNull(nextSeq);
        }
        catch(Exception e) {
            fail(e);
        }
    }

    @Test
    void GetNextRegexSequenceNumber() {
        var user = UserContext.getUser();
        user.setTimeZoneId("IND");

        LocalDateTime olderDate = LocalDateTime.now().plusYears(1L);
        ProductSequenceConfig sequenceSettings = new ProductSequenceConfig();
        sequenceSettings.setSerialCounter(1);
//        sequenceSettings.setSequenceStartTime(olderDate);
        String resetFreq = "daily";

        try {
            var nextSeq = getNextNumberHelper.GetNextRegexSequenceNumber(sequenceSettings, resetFreq);
            assertNotNull(nextSeq);
        }
        catch(Exception e) {
            fail(e);
        }
    }

    @Test
    void getProductSequence(){
        Long productId = 1L;
        ProductProcessTypes productProcessTypes = ProductProcessTypes.ALL;

        ProductSequenceConfig config = new ProductSequenceConfig();
        Page<ProductSequenceConfig> responsePage = new PageImpl<>(List.of(config));

        when(productSequenceConfigDao.findAndLock(any(), any())).thenReturn(config);
        var response = getNextNumberHelper.getProductSequence(productId, productProcessTypes);

        assertNotNull(response);
    }

    @Test
    void getProductSequenceReturnsNull(){
        Long productId = 1L;
        ProductProcessTypes productProcessTypes = ProductProcessTypes.ALL;

        ProductSequenceConfig config = new ProductSequenceConfig();
        Page<ProductSequenceConfig> responsePage = new PageImpl<>(Collections.emptyList());

        when(productSequenceConfigDao.findAndLock(any(), any())).thenReturn(null);
        var response = getNextNumberHelper.getProductSequence(productId, productProcessTypes);

        assertNull(response);
    }

    @Test
    void testPadLeft_InputStringLongerThanOrEqualToLength_ReturnsInputString() {
        String input = "hello";
        int len = 5;
        char c = '*';
        String expected = "hello";
        String result = getNextNumberHelper.padLeft(input, len, c);
        assertEquals(expected, result);
    }

    @Test
    void testPadLeft_InputStringShorterThanLength_ReturnsPaddedString() {
        String input = "hello";
        int len = 10;
        char c = '*';
        String expected = "*****hello";
        String result = getNextNumberHelper.padLeft(input, len, c);
        assertEquals(expected, result);
    }

    @Test
    void testPadLeft_InputStringShorterThanLengthWithDifferentPaddingChar_ReturnsPaddedString() {
        String input = "world";
        int len = 10;
        char c = '-';
        String expected = "-----world";
        String result = getNextNumberHelper.padLeft(input, len, c);
        assertEquals(expected, result);
    }


}