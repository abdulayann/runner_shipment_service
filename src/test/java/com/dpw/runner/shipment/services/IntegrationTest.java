package com.dpw.runner.shipment.services;

import com.dpw.runner.shipment.services.dto.request.Criteria;
import com.dpw.runner.shipment.services.dto.request.FilterCriteria;
import com.dpw.runner.shipment.services.dto.request.Pageable;
import com.dpw.runner.shipment.services.dto.request.SortRequest;
import com.dpw.runner.shipment.services.dto.response.RunnerResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.service.IShipmentService;
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
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
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
        Pageable pageable = createSamplePageable1();
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
        Pageable pageable = createSamplePageable2();
        Page<ShipmentDetails> shipments = new PageImpl<ShipmentDetails>(Collections.emptyList());

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        assertTrue(((List<ShipmentDetails>)objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size() == 0);

    }

    private final String response = "";

    private Pageable createSamplePageable1(){
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
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
                                .build()))
                .build();

        return pageable;
    }

    private Pageable createSamplePageable2(){
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
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
                                .build()))
                .build();

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

    @Test
    public void testFetchByQuery_WithAssignedToFilter_Equals() throws Exception {
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("houseBill")
                        .order("ASC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator("=")
                                                        .value(123)
                                                        .build())
                                                .build()
                                ))
                                .build()))
                .build();

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        RunnerResponse response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class);
        assertTrue(response != null);
    }

    @Test
    public void testFetchByQuery_WithAssignedToFilter_GreaterThan() throws Exception {
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("houseBill")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator(">")
                                                        .value(100)
                                                        .build())
                                                .build()
                                ))
                                .build()))
                .build();

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        RunnerResponse response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class);
        assertTrue(response != null);
    }

    @Test
    public void testFetchByQuery_WithAssignedToFilter_LessThan() throws Exception {
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("houseBill")
                        .order("ASC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator("<")
                                                        .value(500)
                                                        .build())
                                                .build()
                                ))
                                .build()))
                .build();

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        RunnerResponse response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class);
        assertTrue(response != null);
    }

    @Test
    public void testFetchByQuery_WithAssignedToFilter_NotEquals() throws Exception {
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("houseBill")
                        .order("ASC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator("!=")
                                                        .value(999)
                                                        .build())
                                                .build()
                                ))
                                .build()))
                .build();

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        RunnerResponse response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class);
        assertTrue(response != null);
    }

    @Test
    public void testFetchByQuery_WithAssignedToFilter_In() throws Exception {
        List<Integer> assignedToValues = Arrays.asList(100, 200, 300);

        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("houseBill")
                        .order("ASC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator("IN")
                                                        .value(assignedToValues)
                                                        .build())
                                                .build()
                                ))
                                .build()))
                .build();

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        RunnerResponse response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class);
        assertTrue(response != null);
    }

    @Test
    public void testFetchByQuery_WithAssignedToAndTransportModeFilters() throws Exception {
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("houseBill")
                        .order("ASC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("transportMode")
                                                        .operator("=")
                                                        .value("AIR")
                                                        .build())
                                                .build(),
                                        FilterCriteria.builder()
                                                .logicOperator("and")
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator("=")
                                                        .value(456)
                                                        .build())
                                                .build()
                                ))
                                .build()))
                .build();

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        RunnerResponse response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class);
        assertTrue(response != null);
    }

    @Test
    public void testFetchByQuery_WithAssignedToFilter_Range() throws Exception {
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("houseBill")
                        .order("ASC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator(">")
                                                        .value(100)
                                                        .build())
                                                .build(),
                                        FilterCriteria.builder()
                                                .logicOperator("and")
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator("<")
                                                        .value(500)
                                                        .build())
                                                .build()
                                ))
                                .build()))
                .build();

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        RunnerResponse response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class);
        assertTrue(response != null);
    }

    @Test
    public void testFetchByQuery_WithAssignedToFilter_OrCondition() throws Exception {
        Pageable pageable = Pageable.builder()
                .pageNo(0)
                .limit(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("houseBill")
                        .order("ASC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator("=")
                                                        .value(100)
                                                        .build())
                                                .build(),
                                        FilterCriteria.builder()
                                                .logicOperator("or")
                                                .criteria(Criteria.builder()
                                                        .fieldName("assignedTo")
                                                        .operator("=")
                                                        .value(200)
                                                        .build())
                                                .build()
                                ))
                                .build()))
                .build();

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(pageable)))
                .andExpect(status().isOk())
                .andReturn();

        RunnerResponse response = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class);
        assertTrue(response != null);
    }

}
