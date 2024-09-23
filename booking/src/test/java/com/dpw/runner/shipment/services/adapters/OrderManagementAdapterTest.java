package com.dpw.runner.shipment.services.adapters;

import com.dpw.runner.shipment.services.adapters.impl.OrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementDTO;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementResponse;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.QuantityPair;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.V2AuthHelper;
import com.fasterxml.jackson.core.type.TypeReference;
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
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class OrderManagementAdapterTest {

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private IV1Service v1Service;

    @Mock
    private V2AuthHelper v2AuthHelper;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private OrderManagementAdapter orderManagementAdapter;

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
    void getOrderForBookingException() throws RunnerException {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .supplierCode("supCode")
                .buyerCode("buyCode")
                .notifyPartyCode("notifyCode")
                .sendingAgentCode("sendingCode")
                .receivingAgentCode("receivingCode")
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        assertThrows(RunnerException.class, () -> {
            orderManagementAdapter.getOrderForBooking("1234");
        });
    }

    @Test
    void getOrderForBooking() throws RunnerException {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .supplierCode("supCode")
                .buyerCode("buyCode")
                .notifyPartyCode("notifyCode")
                .sendingAgentCode("sendingCode")
                .receivingAgentCode("receivingCode")
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        List<Map<String, Object>> responseMap = new ArrayList<>();

        Map<String, Object> hm = new HashMap<>();

        Map<String, Object> hm1 = new HashMap<>();
        Map<String, Object> hm2 = new HashMap<>();
        Map<String, Object> hm3 = new HashMap<>();

        hm1.put("OrganizationCode", "supCode");
        hm2.put("OrganizationCode", "buyCode");
        hm3.put("OrganizationCode", "notifyCode");

        hm.put("supCode", hm1);
        hm.put("buyCode", hm2);
        hm.put("notifyCode", hm3);

        responseMap.add(hm1);
        responseMap.add(hm2);
        responseMap.add(hm3);

        doReturn(responseMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        assertNotNull(orderManagementAdapter.getOrderForBooking("123"));
    }
}