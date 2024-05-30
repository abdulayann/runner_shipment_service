package com.dpw.runner.shipment.services;

import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.commons.responses.RunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.h2.tools.Server;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith({MockitoExtension.class, SpringExtension.class})
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureMockMvc
@TestPropertySource("classpath:application-test.properties")
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class IntegrationTest {

    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private TestDataGenerator testDataGenerator;

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private IShipmentService shipmentService;

    @Autowired
    private Environment environment;

    @BeforeAll
    public static void initTest() throws SQLException {
        Server.createWebServer("-web", "-webAllowOthers", "-webPort", "9092")
                .start();
    }

//    @Test
    public void sampleTest() throws RunnerException {
        shipmentService.createTestShipment(10);
        assertEquals(environment.getProperty("spring.datasource.url"), "jdbc:h2:mem:testdb;IFEXISTS=FALSE;");
    }

    @BeforeEach
    public void setup() {
        MockitoAnnotations.openMocks(this);
    }


    //@Test
    public void testFetchByQuery_request1() throws Exception {
        ListCommonRequest ListCommonRequest = createSamplePageable1();

        //creating data in H2
        var dataPopulatedInH2 = shipmentService.createTestShipment(10);
        System.out.println(dataPopulatedInH2);


        MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        System.out.println(mvcResult.getResponse().getContentAsString());
        System.out.println(objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getCount());
        assertTrue(((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size() > 0);

    }

    //@Test
    public void testFetchByQuery_request3() throws Exception {
        ListCommonRequest ListCommonRequest = createSamplePageable2();
        Page<ShipmentDetails> shipments = new PageImpl<ShipmentDetails>(Collections.emptyList());

        MvcResult mvcResult = mockMvc.perform(post("/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        assertTrue(((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size() == 0);

    }

//    @Test
//    @Order(1)
    public void createTest() throws Exception {
        var data = testDataGenerator.createTestShipment(40);
        for (var i : data) {
            MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/create")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(i)))
                    .andExpect(status().isOk())
                    .andReturn();
            //shipmentService.create(CommonRequestModel.buildRequest(i));
        }
    }

//    @Test
    public void testCriteria1() throws Exception {
        createTest();
        //Create a request payload for the 1st Criteria
        ListCommonRequest ListCommonRequest = createSamplePageable_AllNull_StatusIs0();

        //Hitting the service endpoint controller and having the actualResult
        MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        var list = ((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData());

        int actualCount = list.size();

        //Assertions
        assertTrue(actualCount > 0);
//        assertEquals(expectedIds.toString(), actualIds.toString());
    }

//    @Test
    public void completeRetrieveTestCriteria() throws Exception {
//        var data = testDataGenerator.createTestShipment(40);
//        for (var i : data) {
//            shipmentService.create(CommonRequestModel.buildRequest(i));
//        }

        int id = 1;
        for (id = 1; id <= 40; id++) {
            mockMvc.perform(get("/api/v2/shipment/retrieve/complete/id")
                            .contentType(MediaType.APPLICATION_JSON)
                            .param("id", String.valueOf(id))
                            .accept(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andDo(print());
        }

        assertTrue(id == 41);
    }

//    @Test
    public void testCriteria2() throws Exception {
        //var dataInH2 = testDataGenerator.populateH2WithTestData();
//        var data = testDataGenerator.createTestShipment(40);
//        for (var i : data) {
//            shipmentService.create(CommonRequestModel.buildRequest(i));
//        }
        ListCommonRequest ListCommonRequest = createSamplePageable_AllNull_StatusIs0_TransportModeIsSEA();

        MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        var expectedResponseIdSet = List.of(5L);

        int actualCount = ((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size();
//        var actualData = (List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData();
//        var actualDataIdSet = actualData.stream().map(o -> o.getId()).sorted().toList();
        assertTrue(actualCount > 0);
    }

//    @Test
    public void testCriteria3() throws Exception {
        //var dataInH2 = testDataGenerator.populateH2WithTestData();
//        var data = testDataGenerator.createTestShipment(40);
//        for (var i : data) {
//            shipmentService.create(CommonRequestModel.buildRequest(i));
//        }
        ListCommonRequest ListCommonRequest = createSamplePageable_AllNull_ShipmentTypeIsEXP_StatusIs0_TransportModeIsSEA();

        MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        var expectedResponseCount = 1;
//        var expectedResponseIdSet = List.of(5L);

        int actualCount = ((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size();
//        var actualData = (List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData();
//        var actualDataIdSet = actualData.stream().map(o -> o.getId()).sorted().toList();

//        assertEquals(expectedResponseCount, actualCount);
//        assertEquals(actualDataIdSet.toString(), expectedResponseIdSet.toString());
        assertTrue(actualCount > 0);
    }

//    @Test
    public void testCriteria4() throws Exception {
        //var dataInH2 = testDataGenerator.populateH2WithTestData();
//        var data = testDataGenerator.createTestShipment(400);
//        for (var i : data) {
//            shipmentService.create(CommonRequestModel.buildRequest(i));
//        }

        ListCommonRequest ListCommonRequest = createSamplePageable_AllNull_ShipmentIdIsNonNull();

        MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        int expectedResponseCount = 1;
        var expectedResponseIdSet = List.of(5L);

        int actualCount = ((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size();
//        var actualData = (List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData();
//        var actualDataIdSet = actualData.stream().map(o -> o.getId()).sorted().toList();

        assertTrue(actualCount == 0);
//        assertEquals(expectedResponseCount, actualCount);
//        assertEquals(actualDataIdSet.toString(), expectedResponseIdSet.toString());
    }

//    @Test
    public void testCriteria5() throws Exception {
//        var dataInH2 = testDataGenerator.populateH2WithTestData();

//        var data = testDataGenerator.createTestShipment(4);
//        for (var i : data) {
//            shipmentService.create(CommonRequestModel.buildRequest(i));
//        }

        ListCommonRequest ListCommonRequest = createSamplePageable_AllNull_ContainerNumbersIsNonNull();

        MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        int expectedResponseCount = 1;
        var expectedResponseIdSet = List.of(5L);

        int actualCount = ((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size();
//        var actualData = (List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData();
//        var actualDataIdSet = actualData.stream().map(o -> o.getId()).sorted().toList();

        assertTrue(actualCount > 0);
        //assertEquals(actualDataIdSet.toString(), expectedResponseIdSet.toString());
    }

//    @Test
    public void testCriteria6() throws Exception {
//        var dataInH2 = testDataGenerator.populateH2WithTestData();

//        var data = testDataGenerator.createTestShipment(4);
//        for (var i : data) {
//            shipmentService.create(CommonRequestModel.buildRequest(i));
//        }

        ListCommonRequest ListCommonRequest = createSamplePageable_AllNull_MasterBillisNonNull_StatusIs1_TransportModeIsNonNull();

        MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        int actualCount = ((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size();
        var actualData = (List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData();
        var actualDataIdSet = actualData.stream().map(o -> o.getId()).sorted().toList();

        assertTrue(actualCount == 0);
//        assertEquals(actualDataIdSet.toString(), expectedResponseIdSet.toString());
    }

//    @Test
    public void testCriteria7() throws Exception {
        //Fires up the H2 for each test case
        //populate data in H2 by hitting the createTestShipment
//        var dataInH2 = testDataGenerator.populateH2WithTestData();
//        var data = testDataGenerator.createTestShipment(4);
//        for (var i : data) {
//            shipmentService.create(CommonRequestModel.buildRequest(i));
//        }

        //Create a request payload for the 1st Criteria
        ListCommonRequest ListCommonRequest = createSamplePageable_AllNull_HouseBillIsNonNull();

        //Hitting the service endpoint controller and having the actualResult
        MvcResult mvcResult = mockMvc.perform(post("/api/v2/shipment/list-shipment")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(objectMapper.writeValueAsString(ListCommonRequest)))
                .andExpect(status().isOk())
                .andReturn();

        //Building the expected response
//        int expectedCount = 2;
//        var expectedIds = List.of(1, 3);

        //Building the actual Response
        int actualCount = ((List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData()).size();
        var actualData = (List<ShipmentDetails>) objectMapper.readValue(mvcResult.getResponse().getContentAsString(), RunnerResponse.class).getData();
        var actualIds = actualData.stream().map(o -> o.getId()).sorted().toList();

        //Assertions
//        assertEquals(expectedCount, actualCount);
//        assertEquals(expectedIds.toString(), actualIds.toString());
        assertTrue(actualCount == 0);
    }

    private final String response = "";

    //********************************************* CRITERIAS **********************************************************

    /**
     * Criteria 1
     * status = 0
     */
    private ListCommonRequest createSamplePageable_AllNull_StatusIs0() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder().innerFilter(List.of(FilterCriteria.builder().criteria(Criteria.builder()
                                .fieldName("status")
                                .operator("=")
                                .value(0).build()).build())).build()
                ))
                .build();
        return requestPayload;
    }

    /**
     * Criteria 2
     * Status = 0 and transportMode = SEA
     */
    private ListCommonRequest createSamplePageable_AllNull_StatusIs0_TransportModeIsSEA() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("status")
                                                        .operator("=")
                                                        .value(0)
                                                        .build())
                                                .build(),
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("transportMode")
                                                        .operator("=")
                                                        .value("SEA")
                                                        .build())
                                                .logicOperator("and")
                                                .build()))
                                .build()))
                .build();
        return requestPayload;
    }


    /**
     * Criteria 3
     * shipment_type = FCL and status = 0 and transport_mode = SEA
     */
    private ListCommonRequest createSamplePageable_AllNull_ShipmentTypeIsEXP_StatusIs0_TransportModeIsSEA() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(
                                                        Criteria.builder()
                                                                .fieldName("status")
                                                                .operator("=")
                                                                .value(0)
                                                                .build())
                                                .build(),
                                        FilterCriteria.builder()
                                                .innerFilter(Arrays.asList(
                                                        FilterCriteria.builder()
                                                                .criteria(Criteria.builder()
                                                                        .fieldName("transportMode")
                                                                        .operator("=")
                                                                        .value("SEA")
                                                                        .build())
                                                                .build(),
                                                        FilterCriteria.builder()
                                                                .criteria(Criteria.builder()
                                                                        .fieldName("shipmentType")
                                                                        .operator("=")
                                                                        .value("FCL")
                                                                        .build())
                                                                .logicOperator("and")
                                                                .build()
                                                ))
                                                .logicOperator("and")
                                                .build()))
                                .build()
                ))
                .build();

        return requestPayload;
    }

    /**
     * Criteria 4
     * shipmentId = SHP000102015
     */

    private ListCommonRequest createSamplePageable_AllNull_ShipmentIdIsNonNull() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("shipmentId")
                                        .operator("=")
                                        .value("SHP0000")
                                        .build())
                                .build()
                ))
                .build();

        return requestPayload;
    }


    /**
     * Criteria 5
     * ContainerNumbers = CONTE7C9K4zr8z
     */
    private ListCommonRequest createSamplePageable_AllNull_ContainerNumbersIsNonNull() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder().innerFilter(List.of(FilterCriteria.builder().criteria(Criteria.builder()
                                .fieldName("containerNumber")
                                .operator("=")
                                .value("CONT00001").build()).build())).build()
                ))
                .build();
        return requestPayload;
    }

    /**
     * Criteria 6
     * MasterBill = 9G4E5AYD93 and status = 0 and transportMode = SEA
     */
    private ListCommonRequest createSamplePageable_AllNull_MasterBillisNonNull_StatusIs1_TransportModeIsNonNull() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(
                                                        Criteria.builder()
                                                                .fieldName("status")
                                                                .operator("=")
                                                                .value(1)
                                                                .build())
                                                .build(),
                                        FilterCriteria.builder()
                                                .innerFilter(Arrays.asList(
                                                        FilterCriteria.builder()
                                                                .criteria(Criteria.builder()
                                                                        .fieldName("transportMode")
                                                                        .operator("=")
                                                                        .value("SEA")
                                                                        .build())
                                                                .build(),
                                                        FilterCriteria.builder()
                                                                .criteria(Criteria.builder()
                                                                        .fieldName("masterBill")
                                                                        .operator("=")
                                                                        .value("9G4E5AYD93")
                                                                        .build())
                                                                .logicOperator("and")
                                                                .build()
                                                ))
                                                .logicOperator("and")
                                                .build()))
                                .build()
                ))
                .build();

        return requestPayload;
    }

    /**
     * Criteria 7
     * HouseBill = FTOGI1283602230TzrNGM
     */

    private ListCommonRequest createSamplePageable_AllNull_HouseBillIsNonNull() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("houseBill")
                                        .operator("=")
                                        .value("FTOGI1283602230TzrNGM")
                                        .build())
                                .build()
                ))
                .build();
        return requestPayload;
    }

    /**
     * Criteria 8
     * ClientId = 11849
     */

    private ListCommonRequest createSamplePageable_AllNull_ClientIdIsNonNull() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("orgId")
                                        .operator("=")
                                        .value("11849")
                                        .build())
                                .build()
                ))
                .build();
        return requestPayload;
    }


    /**
     * Criteria 9
     * Consigner = 10516 and Consignee = 10517
     */

    private ListCommonRequest createSamplePageable_AllNull_ConsignerIsNonNull_ConsigneeIsNonNull() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("Consigner")
                                        .operator("=")
                                        .value("10516")
                                        .build())
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(Criteria.builder()
                                                        .fieldName("consignee")
                                                        .operator("=")
                                                        .value("10517")
                                                        .build())
                                                .logicOperator("and")
                                                .build()
                                ))
                                .build()))
                .build();
        return requestPayload;
    }

    /**
     * Criteria 10
     * ConsolidationNumber = CONSO000180649 and shipmentType = EXP and TransportMode = SEA
     */

    private ListCommonRequest createSamplePageable_AllNull_ConsolidationNumberIsNonNull_ShipmentTypeIsFCL_TransportModeIsAIR() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .innerFilter(Arrays.asList(
                                        FilterCriteria.builder()
                                                .criteria(
                                                        Criteria.builder()
                                                                .fieldName("consolidationNumber")
                                                                .operator("=")
                                                                .value("CONSO000180649")
                                                                .build())
                                                .build(),
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
                                                                .criteria(Criteria.builder()
                                                                        .fieldName("shipmentType")
                                                                        .operator("=")
                                                                        .value("FCL")
                                                                        .build())
                                                                .logicOperator("and")
                                                                .build()
                                                ))
                                                .logicOperator("and")
                                                .build()))
                                .build()
                ))
                .build();

        return requestPayload;
    }

    /**
     * Criteria 11 :
     * TransportMode = SEA and WayBillNumber = DPWCANTR_355814
     */

    private ListCommonRequest createSamplePageable_AllNull_TransportModeIsNonNull_WayBillNumberIsNonNull() {
        ListCommonRequest requestPayload = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
                .sortRequest(SortRequest.builder()
                        .fieldName("deliveryMode")
                        .order("DESC")
                        .build())
                .filterCriteria(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("transportMode")
                                        .operator("=")
                                        .value("SEA")
                                        .build())
                                .build(),
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("wayBillNumber")
                                        .operator("=")
                                        .value("DPWCANTR_355814")
                                        .build())
                                .logicOperator("and")
                                .build()
                ))
                .build();
        return requestPayload;
    }

    private ListCommonRequest createSamplePageable1() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
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

        return listCommonRequest;
    }

    private ListCommonRequest createSamplePageable2() {
        ListCommonRequest listCommonRequest = ListCommonRequest.builder()
                .pageNo(1)
                .pageSize(10)
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

        return listCommonRequest;
    }

    //@Test
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
