package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummaryResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.modelmapper.ModelMapper;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class BillingServiceAdapterTest {

    @Mock
    private RestTemplate restTemplate;

    @InjectMocks
    private BillingServiceAdapter billingServiceAdapter;

    @Mock
    private ModelMapper modelMapper;

    @Test
    void fetchActiveInvoicesTestDouble() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();

        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
         (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().totalRevenue(0.001).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(commonGetRequest));
    }

    @Test
    void fetchActiveInvoicesTestInteger() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();

        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().totalCount(1).build());

        assertEquals(true, billingServiceAdapter.fetchActiveInvoices(commonGetRequest));
    }

    @Test
    void fetchActiveInvoicesTestNull() throws RunnerException {
        CommonGetRequest commonGetRequest = CommonGetRequest.builder().guid("3d7ac60d-5ada-4cff-9f4d-2fde960e3e06").build();

        BillingSummaryResponse billingSummaryResponse = new BillingSummaryResponse();
        when(restTemplate.postForEntity(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<Class<Object>>any(),
                (Object[]) any())).thenReturn(ResponseEntity.ok(billingSummaryResponse));
        when(modelMapper.map(any(), any())).thenReturn(BillingSummary.builder().build());

        assertEquals(false, billingServiceAdapter.fetchActiveInvoices(commonGetRequest));
    }


}
