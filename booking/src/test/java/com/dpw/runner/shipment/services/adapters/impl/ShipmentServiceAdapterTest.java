package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.ShipmentServiceConfig;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.interfaces.IQuoteContractsService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
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
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.*;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.client.RestTemplate;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

@ContextConfiguration(classes = {ShipmentServiceAdapter.class, String.class})
@ExtendWith(SpringExtension.class)
@PropertySource("classpath:application-test.properties")
@EnableConfigurationProperties
@Execution(ExecutionMode.CONCURRENT)
class ShipmentServiceAdapterTest {

    @MockBean
    private JsonHelper jsonHelper;

    @MockBean
    private ModelMapper modelMapper;

    @MockBean
    private ShipmentServiceConfig shipmentServiceConfig;

    @Autowired
    private ShipmentServiceAdapter shipmentServiceAdapter;

    @Mock
    private CommonUtils commonUtils;

    @MockBean(name = "restTemplateForShipment")
    private RestTemplate restTemplate;

    public static class ShipmentResponse extends RunnerResponse<ShipmentDetailsResponse>{}

    @BeforeEach
    void setUp() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void createShipmentTest() throws RunnerException {
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        ShipmentDetailsResponse expectedResponseData = ShipmentDetailsResponse.builder().shipmentType("FCL").build();
        RunnerResponse<ShipmentDetailsResponse> runnerResponse = new RunnerResponse<>();
        runnerResponse.setData(expectedResponseData);
        ResponseEntity<RunnerResponse> responseEntity = ResponseEntity.ok(runnerResponse);
        when(restTemplate.exchange(any(RequestEntity.class), any(Class.class))).thenReturn(responseEntity);
        when(jsonHelper.readFromJson(any(), eq(ShipmentDetailsResponse.class))).thenReturn(expectedResponseData);
        ShipmentDetailsResponse response = shipmentServiceAdapter.createShipment(shipmentDetailsResponse);
        assertNotNull(response);
        assertEquals(expectedResponseData, response);
    }


    @Test
    void createShipmentExceptionTest(){
        ShipmentDetailsResponse shipmentDetailsResponse = ShipmentDetailsResponse.builder().build();
        ShipmentDetailsResponse expectedResponseData = ShipmentDetailsResponse.builder().shipmentType("FCL").build();
        RunnerResponse<ShipmentDetailsResponse> runnerResponse = new RunnerResponse<>();
        ResponseEntity<RunnerResponse> responseEntity = ResponseEntity.ok(runnerResponse);
        when(restTemplate.exchange(any(RequestEntity.class), any(Class.class))).thenReturn(responseEntity);
        when(jsonHelper.readFromJson(any(), eq(ShipmentDetailsResponse.class))).thenReturn(expectedResponseData);

        assertThrows(RunnerException.class, () -> {
            shipmentServiceAdapter.createShipment(shipmentDetailsResponse);
        });
    }


    /**
     * Method under test:
     * {@link ShipmentServiceAdapter#getShipmentIdbyGuid(String)}
     */
    @Test
    void getShipmentByGuidTest() throws RunnerException {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(1L);

        ShipmentResponse shipmentResponse = new ShipmentResponse();
        shipmentResponse.setData(shipmentDetailsResponse);

        when(restTemplate.exchange(any(RequestEntity.class), any(Class.class))).thenReturn(ResponseEntity.ok(shipmentResponse));

        ResponseEntity<IRunnerResponse> response = shipmentServiceAdapter.getShipmentIdbyGuid("abcd");

        // Validate the response
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(((RunnerResponse<?>) response.getBody()).getData());
    }


    @Test
    void getShipmentByGuidException() {
        ShipmentDetailsResponse shipmentDetailsResponse = new ShipmentDetailsResponse();
        shipmentDetailsResponse.setId(1L);
        RunnerResponse<ShipmentDetailsResponse> response = new RunnerResponse();
        response.setData(shipmentDetailsResponse);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, ShipmentDetailsResponse.class);
        assertThrows(RunnerException.class, () -> {
            shipmentServiceAdapter.getShipmentIdbyGuid("1234");
        });
    }

}
