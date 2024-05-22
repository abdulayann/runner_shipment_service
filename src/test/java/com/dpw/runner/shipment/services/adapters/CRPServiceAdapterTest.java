package com.dpw.runner.shipment.services.adapters;

import com.dpw.runner.shipment.services.adapters.config.CRPConfig;
import com.dpw.runner.shipment.services.adapters.impl.CRPServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.crp.CRPListRequest;
import com.dpw.runner.shipment.services.dto.request.crp.CRPRetrieveRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

//@ExtendWith(MockitoExtension.class)
//@Execution(ExecutionMode.CONCURRENT)
@SpringBootTest(classes = {CRPConfig.class, CRPServiceAdapter.class})
class CRPServiceAdapterTest {

    @MockBean
    @Qualifier("restTemplateForCRP")
    private RestTemplate restTemplate;

    @InjectMocks
    private CRPServiceAdapter crpServiceAdapter;

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
    void retrieveCRPService() throws RunnerException {
        CRPRetrieveRequest request= new CRPRetrieveRequest("url");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<?> responseEntity_ = new ResponseEntity<>(new Object(), HttpStatus.OK);
        when(restTemplate.exchange(eq(RequestEntity.get(URI.create("nullurl")).build()), eq(Object.class))).thenReturn((ResponseEntity<Object>) responseEntity_);
        assertThrows(NullPointerException.class, () -> crpServiceAdapter.retrieveCRPService(commonRequestModel));
//        assertNotNull(responseEntity);
//        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void retrieveCRPService_() throws RunnerException {
        CRPRetrieveRequest request= new CRPRetrieveRequest("url");
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<?> responseEntity_ = new ResponseEntity<>(new Object(), HttpStatus.OK);
        when(restTemplate.exchange(any(), eq(Object.class))).thenAnswer(invocation -> {
            return responseEntity_;
        });
        ResponseEntity<IRunnerResponse> responseEntity = crpServiceAdapter.retrieveCRPService(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

    @Test
    void listCRPService() throws RunnerException {
        CRPListRequest request= new CRPListRequest();
        CommonRequestModel commonRequestModel = CommonRequestModel.buildRequest(request);
        ResponseEntity<?> responseEntity_ = new ResponseEntity<>(new Object(), HttpStatus.OK);
        doReturn(responseEntity_).when(restTemplate).exchange(any(), eq(Object.class));
        ResponseEntity<IRunnerResponse> responseEntity = crpServiceAdapter.listCRPService(commonRequestModel);
        assertNotNull(responseEntity);
        assertEquals(HttpStatus.OK, responseEntity.getStatusCode());
    }

}
