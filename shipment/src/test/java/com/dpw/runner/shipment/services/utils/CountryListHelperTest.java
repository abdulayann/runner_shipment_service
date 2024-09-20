package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class CountryListHelperTest {

    @InjectMocks
    private CountryListHelper countryListHelper;

    private static JsonTestUtility jsonTestUtility;

    private static ObjectMapper objectMapper;
    @BeforeAll
    static void init() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapper = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
    }

    @Test
    void testFromAlpha3_Success() {
        CountryListHelper.ISO3166Country country = CountryListHelper.ISO3166.fromAlpha3("ALB");
        assertEquals("AL", country.getAlpha2());
        assertEquals("Albania", country.getName());
    }

    @Test
    void testFromAlpha2_Success() {
        CountryListHelper.ISO3166Country country = CountryListHelper.ISO3166.fromAlpha2("DZ");
        assertEquals("DZA", country.getAlpha3());
        assertEquals("Algeria", country.getName());
    }

    @Test
    void testGetCountryNameByCode_Success_2Digit() {
        String countryName = CountryListHelper.ISO3166.getCountryNameByCode("DZ");
        assertEquals("Algeria", countryName);
    }

    @Test
    void testGetCountryNameByCode_Success_3Digit() {
        String countryName = CountryListHelper.ISO3166.getCountryNameByCode("DZA");
        assertEquals("Algeria", countryName);
    }

    @Test
    void testGetCountryNameByCode_Success_4Digit() {
        String countryName = CountryListHelper.ISO3166.getCountryNameByCode("DZAA");
        assertEquals("", countryName);
    }
}
