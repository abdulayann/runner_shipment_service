package com.dpw.runner.shipment.services.controller;

import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.dto.request.ListContractRequest;
import com.dpw.runner.shipment.services.dto.request.ListContractsWithFilterRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMAutoSellRequest;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchOffersRequestFromUI;
import com.dpw.runner.shipment.services.dto.request.npm.NPMImportRatesRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ContextConfiguration;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {MasterDataController.class})
@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class NPMControllerTest {

    @Mock
    private INPMServiceAdapter npmService;
    @Mock
    private JsonHelper jsonHelper;
    @InjectMocks
    private NPMController npmController;

    @Test
    void fetchContractFromShipment() throws RunnerException {
        // Mock
        when(npmService.fetchContractFromShipment(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = npmController.fetchContractFromShipment(ListContractRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchContractFromShipment2() throws RunnerException {
        // Mock
        when(npmService.fetchContractFromShipment(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = npmController.fetchContractFromShipment(ListContractRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchContractFromShipment3() throws RunnerException {
        // Mock
        when(npmService.fetchContractFromShipment(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = npmController.fetchContractFromShipment(ListContractRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchContracts() throws RunnerException {
        // Mock
        when(npmService.fetchContracts(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = npmController.fetchContracts(ListContractsWithFilterRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchContracts2() throws RunnerException {
        // Mock
        when(npmService.fetchContracts(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = npmController.fetchContracts(ListContractsWithFilterRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchContracts3() throws RunnerException {
        // Mock
        when(npmService.fetchContracts(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = npmController.fetchContracts(ListContractsWithFilterRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchContract() throws RunnerException {
        // Mock
        when(npmService.fetchContract(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = npmController.fetchContract(ListContractRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void fetchContract2() throws RunnerException {
        // Mock
        when(npmService.fetchContract(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = npmController.fetchContract(ListContractRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void fetchContract3() throws RunnerException {
        // Mock
        when(npmService.fetchContract(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = npmController.fetchContract(ListContractRequest.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getNPMOffers() throws RunnerException {
        // Mock
        when(npmService.fetchOffers(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = npmController.getNPMOffers(NPMFetchOffersRequestFromUI.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getNPMOffers2() throws RunnerException {
        // Mock
        when(npmService.fetchOffers(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = npmController.getNPMOffers(NPMFetchOffersRequestFromUI.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getNPMOffers3() throws RunnerException {
        // Mock
        when(npmService.fetchOffers(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = npmController.getNPMOffers(NPMFetchOffersRequestFromUI.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getNPMOffersV8() throws RunnerException {
        // Mock
        when(npmService.fetchOffersV8(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = npmController.getNPMOffersV8(NPMFetchOffersRequestFromUI.builder().build());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getNPMOffersV82() throws RunnerException {
        // Mock
        when(npmService.fetchOffersV8(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = npmController.getNPMOffersV8(NPMFetchOffersRequestFromUI.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getNPMOffersV83() throws RunnerException {
        // Mock
        when(npmService.fetchOffersV8(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = npmController.getNPMOffersV8(NPMFetchOffersRequestFromUI.builder().build());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAwbAutoSell() throws RunnerException {
        // Mock
        when(npmService.awbAutoSell(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = npmController.getAwbAutoSell(new NPMAutoSellRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAwbAutoSell2() throws RunnerException {
        // Mock
        when(npmService.awbAutoSell(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = npmController.getAwbAutoSell(new NPMAutoSellRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAwbAutoSell3() throws RunnerException {
        // Mock
        when(npmService.awbAutoSell(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = npmController.getAwbAutoSell(new NPMAutoSellRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAwbImportRates() throws RunnerException {
        // Mock
        when(npmService.awbImportRates(any())).thenReturn(ResponseHelper.buildSuccessResponse());
        // Test
        var responseEntity = npmController.getAwbImportRates(new NPMImportRatesRequest());
        // Assert
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void getAwbImportRates2() throws RunnerException {
        // Mock
        when(npmService.awbImportRates(any())).thenThrow(new RuntimeException());
        // Test
        var responseEntity = npmController.getAwbImportRates(new NPMImportRatesRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

    @Test
    void getAwbImportRates3() throws RunnerException {
        // Mock
        when(npmService.awbImportRates(any())).thenThrow(new RuntimeException("RuntimeException"));
        // Test
        var responseEntity = npmController.getAwbImportRates(new NPMImportRatesRequest());
        // Assert
        assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());
    }

}
