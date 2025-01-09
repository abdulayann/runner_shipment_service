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
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
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
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class QuoteContractsServiceTest {

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
        verify(quoteContractsDao, Mockito.times(1)).save(any());
    }

    @Test
    void updateQuoteContracts_NullRequest() {
        quoteContractsService.updateQuoteContracts(null);
        verify(quoteContractsDao, Mockito.times(0)).save(any());
    }

    @Test
    void updateQuoteContracts_NullContract() {
        ListContractResponse listContractResponse = jsonTestUtility.getListContractResponse();
        listContractResponse.setContracts(null);
        quoteContractsService.updateQuoteContracts(listContractResponse);
        verify(quoteContractsDao, Mockito.times(0)).save(any());
    }

    @Test
    void updateQuoteContracts_error() {
        ListContractResponse listContractResponse = jsonTestUtility.getListContractResponse();
        when(quoteContractsDao.save(any())).thenThrow(new ValidationException(""));
        quoteContractsService.updateQuoteContracts(listContractResponse);
        verify(quoteContractsDao, Mockito.times(1)).save(any());
    }

    @Test
    void updateQuoteContracts_oldQuoteContracts() {
        ListContractResponse listContractResponse = jsonTestUtility.getListContractResponse();
        listContractResponse.getContracts().get(0).getContract_usage().get(0).setFilter_params(null);
        when(quoteContractsDao.findByContractId(anyString())).thenReturn(List.of(testQuoteContracts));
        quoteContractsService.updateQuoteContracts(listContractResponse);
        verify(quoteContractsDao, Mockito.times(1)).save(any());
    }

    @Test
    void updateQuoteContracts_NullContractUsages() {
        ListContractResponse listContractResponse = jsonTestUtility.getListContractResponse();
        listContractResponse.getContracts().get(0).setContract_usage(null);
        quoteContractsService.updateQuoteContracts(listContractResponse);
        verify(quoteContractsDao, Mockito.times(1)).save(any());
    }

    @Test
    void updateQuoteContracts_NullContractUsage() {
        ListContractResponse listContractResponse = jsonTestUtility.getListContractResponse();
        listContractResponse.getContracts().get(0).getContract_usage().set(0, null);
        quoteContractsService.updateQuoteContracts(listContractResponse);
        verify(quoteContractsDao, Mockito.times(1)).save(any());
    }

    @Test
    void updateQuoteContracts_NullFilterParams() {
        ListContractResponse listContractResponse = jsonTestUtility.getListContractResponse();
        listContractResponse.getContracts().get(0).getContract_usage().get(0).setFilter_params(null);
        quoteContractsService.updateQuoteContracts(listContractResponse);
        verify(quoteContractsDao, Mockito.times(1)).save(any());
    }

    @Test
    void updateQuoteContracts_NullCargoTypes() {
        ListContractResponse listContractResponse = jsonTestUtility.getListContractResponse();
        listContractResponse.getContracts().get(0).getContract_usage().get(0).getFilter_params().setCargo_type(null);
        quoteContractsService.updateQuoteContracts(listContractResponse);
        verify(quoteContractsDao, Mockito.times(1)).save(any());
    }

    @Test
    void getQuoteContractsByContractId_NullResponse() {
        assertNull(quoteContractsService.getQuoteContractsByContractId(null));
    }

    @Test
    void getQuoteContractsByContractId() {
        String contractId = "contract";
        var quoteContractsResponse = List.of(QuoteContracts.builder().build());
        when(quoteContractsDao.findByContractId(anyString())).thenReturn(quoteContractsResponse);

        var response = quoteContractsService.getQuoteContractsByContractId(contractId);
        assertNotNull(response);

        verify(quoteContractsDao, Mockito.times(1)).findByContractId(anyString());
    }

    @Test
    void getQuoteContractsByContractId_EmptyList() {
        String contractId = "contract";
        when(quoteContractsDao.findByContractId(anyString())).thenReturn(Collections.emptyList());

        quoteContractsService.getQuoteContractsByContractId(contractId);

        verify(quoteContractsDao, Mockito.times(1)).findByContractId(anyString());
    }

}
