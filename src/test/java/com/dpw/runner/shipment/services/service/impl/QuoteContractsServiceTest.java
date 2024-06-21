package com.dpw.runner.shipment.services.service.impl;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.impl.QuoteContractsDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.dto.response.QuoteContractsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.QuoteContracts;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
public class QuoteContractsServiceTest {

    @Mock
    private JsonHelper jsonHelper;

    @Mock
    private QuoteContractsDao quoteContractsDao;

    @InjectMocks
    private QuoteContractsService quoteContractsService;

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
    void list() {
        QuoteContractsResponse quoteContractsResponse = objectMapperTest.convertValue(testQuoteContracts, QuoteContractsResponse.class);
        testQuoteContracts.setId(1L);
        ListCommonRequest getRequest = ListCommonRequest.builder().build();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(getRequest);
        Page<QuoteContracts> page = new PageImpl<>(List.of(testQuoteContracts) , PageRequest.of(0 , 10) , 1);

        when(quoteContractsDao.findAll(any(), any())).thenReturn(page);
        when(jsonHelper.convertValue(any(QuoteContracts.class), eq(QuoteContractsResponse.class))).thenReturn((QuoteContractsResponse) quoteContractsResponse);

        ResponseEntity<IRunnerResponse> responseEntity = quoteContractsService.list(commonRequestModel);

        assertNotNull(responseEntity);
        assertEquals(ResponseHelper.buildListSuccessResponse(List.of(quoteContractsResponse), page.getTotalPages(), page.getTotalElements()), responseEntity);
    }

    @Test
    void testList_Failure() {
        ResponseEntity<IRunnerResponse> httpResponse = quoteContractsService.list(CommonRequestModel.buildRequest());
        assertNotNull(httpResponse);
        assertEquals(HttpStatus.BAD_REQUEST, httpResponse.getStatusCode());
    }

    @Test
    void create() {
        assertNull(quoteContractsService.create(CommonRequestModel.buildRequest()));
    }

    @Test
    void update() throws Exception{
        assertNull(quoteContractsService.update(CommonRequestModel.buildRequest()));
    }

    @Test
    void listAsync() {
        assertNull(quoteContractsService.listAsync(CommonRequestModel.buildRequest()));
    }

    @Test
    void delete() throws Exception{
        assertNull(quoteContractsService.delete(CommonRequestModel.buildRequest()));
    }

    @Test
    void retrieveById() throws Exception{
        assertNull(quoteContractsService.retrieveById(CommonRequestModel.buildRequest()));
    }

    @Test
    void updateQuoteContracts() {
        ListContractResponse listContractResponse = jsonTestUtility.getListContractResponse();
        quoteContractsService.updateQuoteContracts(listContractResponse);
    }

}
