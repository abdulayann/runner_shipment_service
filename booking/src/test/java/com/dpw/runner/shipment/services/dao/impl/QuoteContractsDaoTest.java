package com.dpw.runner.shipment.services.dao.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.QuoteContracts;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.repository.interfaces.IQuoteContractsRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;
import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class QuoteContractsDaoTest {

    @Mock
    private IQuoteContractsRepository quoteContractsRepository;

    @InjectMocks
    private QuoteContractsDao quoteContractsDao;

    private static QuoteContracts testQuoteContracts;
    private static JsonTestUtility jsonTestUtility;
    private static ObjectMapper objectMapperTest;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        testQuoteContracts = jsonTestUtility.getQuoteContracts();
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void save() {
        Mockito.when(quoteContractsRepository.save(ArgumentMatchers.any())).thenReturn(testQuoteContracts);
        QuoteContracts quoteContracts = quoteContractsDao.save(testQuoteContracts);
        assertEquals(testQuoteContracts, quoteContracts);
    }

    @Test
    void findByContractId() {
        Mockito.when(quoteContractsRepository.findByContractId(ArgumentMatchers.any())).thenReturn(List.of(testQuoteContracts));
        List<QuoteContracts> quoteContracts = quoteContractsDao.findByContractId("test");
        assertEquals(testQuoteContracts, quoteContracts.get(0));
    }

    @Test
    void testFindAll() {
        Page<QuoteContracts> routingsPage = mock(Page.class);
        Specification<QuoteContracts> spec = mock(Specification.class);
        Pageable pageable = mock(Pageable.class);
        Mockito.when(quoteContractsRepository.findAll(spec, pageable)).thenReturn(routingsPage);

        Page<QuoteContracts> foundQuoteContractsPage = quoteContractsDao.findAll(spec, pageable);

        assertEquals(routingsPage, foundQuoteContractsPage);
    }
}
