package com.dpw.runner.shipment.services;

import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureMockMvc
public class IntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private IShipmentService shipmentService;

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }


    @Test
    public void testFetchByQuery_request1() throws Exception {
        ListCommonRequest pageable = createSamplePageable1();
        Page<ShipmentDetails> shipments = new PageImpl<ShipmentDetails>(Collections.emptyList());

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        System.out.println(mvcResult.getResponse().getContentAsString());
        System.out.println(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getCount());
        assertTrue(((List<ShipmentDetails>)objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size() > 0);

    }

    @Test
    public void testFetchByQuery_request3() throws Exception {
        ListCommonRequest pageable = createSamplePageable2();
        Page<ShipmentDetails> shipments = new PageImpl<ShipmentDetails>(Collections.emptyList());

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        assertTrue(((List<ShipmentDetails>)objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size() == 0);

    }

    private final String response = "";

    private ListCommonRequest createSamplePageable1(){
        ListCommonRequest pageable = new ListCommonRequest();

        pageable.setPageNo(0);
        pageable.setLimit(10);
        pageable.setSortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build());
        pageable.setFilterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                .fieldName("transportMode")
                                                .operator("=")
                                                .value("AIR")
                                                .build()).build(),
                                        FilterCriteria.builder()
                                                .logicOperator("or")
                                                .criteria(Criteria.builder()
                                                        .fieldName("transportMode")
                                                        .operator("=")
                                                        .value("SEA")
                                                        .build())
                                                .build()
                                ))
                                .build()));

        return pageable;
    }

    private ListCommonRequest createSamplePageable2(){
        ListCommonRequest pageable = new ListCommonRequest();

        pageable.setPageNo(0);
        pageable.setLimit(10);
        pageable.setSortRequest(SortRequest.builder()
                .fieldName("deliveryMode")
                .order("DESC")
                .build());
        pageable.setFilterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("transportMode")
                                                        .operator("=")
                                                        .value("AIR")
                                                        .build()).build(),
                                        FilterCriteria.builder()
                                                .logicOperator("and")
                                                .criteria(Criteria.builder()
                                                        .fieldName("transportMode")
                                                        .operator("=")
                                                        .value("SEA")
                                                        .build())
                                                .build()
                                ))
                                .build()));

        return pageable;
    }

    @Test
    public void testCreateTestRecord() throws Exception {
        int count = 5;
        List<ShipmentDetails> shipments = new ArrayList<>();

        MvcResult mvcResult = mockMvc.perform(post("/create-test-shipment/{count}", count))
                .andExpect(status().isOk())
                .andReturn();

        String responseContent = mvcResult.getResponse().getContentAsString();
        System.out.println(responseContent);
        assertTrue(!responseContent.isEmpty());
    }

}
