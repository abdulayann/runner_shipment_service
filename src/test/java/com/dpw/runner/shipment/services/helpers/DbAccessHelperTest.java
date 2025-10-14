package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.*;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.enums.AndesStatus;
import com.dpw.runner.shipment.services.entity.enums.CustomerCategoryRates;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
//import org.hibernate.query.criteria.internal.CriteriaBuilderImpl;
//import org.hibernate.query.criteria.internal.CriteriaQueryImpl;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Fetch;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.Path;
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
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;

import jakarta.persistence.criteria.*;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DbAccessHelperTest {

    @InjectMocks
    private DbAccessHelper dbAccessHelper;

    @Mock
    private ListCommonRequest request;

    @Mock
    private SortRequest sortRequest;

    @Mock
    private RunnerEntityMapping runnerEntityMapping;

    private static JsonTestUtility jsonTestUtility;
    private static ListCommonRequest listCommonRequest;
    private static ListCommonRequest listCommonRequest1;
    private static ListCommonRequest listCommonRequest2;
    private static ListCommonRequest listCommonRequestIsEnum;
    private static ObjectMapper objectMapperTest;
    private static final ObjectMapper objectMapper = new ObjectMapper();

    @Mock
    private Root<?> root;

    @Mock
    private CriteriaQuery<?> query;

    @Mock
    private CriteriaQuery<ShipmentDetails> criteriaQuery;

    @Mock
    private CriteriaBuilder criteriaBuilder;

    @Mock
    private Join<ShipmentDetails, ShipmentDetails> mockJoin;

//    @Mock
//    private CriteriaBuilderImpl criteriaBuilderImpl;

    @Mock
    private Path<?> path;

    @Mock
    private Path<UUID> path1;

    @Mock
    private Join<?, ?> join;

    @Mock
    private Fetch<?, ?> fetch;

    @BeforeAll
    static void beforeAll() throws IOException {
        jsonTestUtility = new JsonTestUtility();
        objectMapperTest = JsonTestUtility.getMapper();
    }

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        TenantContext.setCurrentTenant(1);
        UserContext.setUser(UsersDto.builder().Username("user").TenantId(1).Permissions(new HashMap<>()).build()); // Set up a mock user for testing
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().build());

        listCommonRequest = jsonTestUtility.getListRequest();
        listCommonRequest1 = jsonTestUtility.getListRequest1();
        listCommonRequest2 = jsonTestUtility.getListRequest2();
        listCommonRequestIsEnum = jsonTestUtility.getListRequestIsEnum();
        // Use Mockito to create mock CriteriaQuery instead of internal classes
        query = mock(CriteriaQuery.class);
    }

    @Test
    void fetchDataTableNamesNull() {
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }

    @Test
    void fetchDataTableNamesNotNullDoesNotContainTextSortRequestNotNull() {
        ListCommonRequest tempReq = listCommonRequest;
        tempReq.getFilterCriteria().get(0).setInnerFilter(Collections.emptyList());
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName("shipment_details").build());
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }

    @Test
    void fetchDataSortRequest() {
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class));
    }

    @Test
    void fetchDataTableNamesNotNullDoesNotContainText() {
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName("shipment_details").build());
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }

    @Test
    void fetchDataTableNamesNotNull() {
        listCommonRequest.setContainsText("transportMode");
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).build());
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }

    @Test
    void fetchDataTableNamesNotNullLogicalOperator() {
        listCommonRequest.setContainsText("transportMode");
        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).build());
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames));
    }

    @Test
    void fetchDataTableNamesNotNullPredicate_NotInLong_UUID_Date() {
        listCommonRequest.setContainsText("transportMode");
        listCommonRequest.setContainsText("status");
        listCommonRequest.setContainsText("eta");
        listCommonRequest.setContainsText("etd");
        listCommonRequest.setContainsText("masterBill");
        listCommonRequest.setContainsText("origin");
        listCommonRequest.setContainsText("destination");
        listCommonRequest = CommonUtils.andCriteria("id", List.of(1), "NOTIN", listCommonRequest);
        listCommonRequest = CommonUtils.andCriteria("guid", List.of(UUID.randomUUID()), "NOTIN", listCommonRequest);
        listCommonRequest = CommonUtils.andCriteria("customerCategory", List.of(CustomerCategoryRates.CATEGORY_1), "NOTIN", listCommonRequest);
        listCommonRequest = CommonUtils.andCriteria("eta", List.of(LocalDateTime.now()), "NOTIN", listCommonRequest);

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).isContainsText(true).dataType(String.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).isContainsText(true).dataType(String.class).build());
        tableNames.put("eta", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).isContainsText(true).dataType(String.class).build());
        tableNames.put("masterBill", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).isContainsText(true).dataType(List.class).build());
        tableNames.put("origin", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).isContainsText(true).dataType(String.class).build());
        tableNames.put("destination", RunnerEntityMapping.builder().tableName(Constants.CARRIER_DETAILS).isContainsText(true).dataType(String.class).build());
        tableNames.put("id", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).isContainsText(true).dataType(Long.class).build());
        tableNames.put("guid", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).isContainsText(true).dataType(UUID.class).build());
        tableNames.put("customerCategory", RunnerEntityMapping.builder().tableName(Constants.SHIPMENT_DETAILS).isContainsText(true).dataType(CustomerCategoryRates.class).build());


        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
        CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
        CriteriaBuilder.In<Object> inClause = mock(CriteriaBuilder.In.class);
        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join(Constants.CARRIER_DETAILS, JoinType.LEFT)).thenReturn((Join) join);
        when(criteriaBuilder.in(path.get("id"))).thenReturn(inClause);
        when(criteriaBuilder.in(path.get("guid"))).thenReturn(inClause);
        when(criteriaBuilder.in(path.get("customerCategory"))).thenReturn(inClause);
        when(criteriaBuilder.in(path.get("eta"))).thenReturn(inClause);
        when(inClause.value(anyList())).thenReturn(inClause);

        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullPredicateGuid() {
        listCommonRequest1.setContainsText("status");
        listCommonRequest1.setContainsText("guid");
        listCommonRequest1.setContainsText("eta");
        listCommonRequest1.setContainsText("etd");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Integer.class).build());
        tableNames.put("guid", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(UUID.class).build());
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Date.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Date.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest1, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
//        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
//        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);
//
//        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
//        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullPredicateLocalDateTime() {
        listCommonRequest1.setContainsText("status");
        listCommonRequest1.setContainsText("guid");
        listCommonRequest1.setContainsText("eta");
        listCommonRequest1.setContainsText("etd");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Integer.class).build());
        tableNames.put("guid", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(UUID.class).build());
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest1, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
//        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
//        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);
//
//        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
//        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullPredicateLocalDateTimeIsEnum() {
        listCommonRequestIsEnum.setContainsText("status");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Enum.class).build());
        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequestIsEnum, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
//        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
//        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);
//
//        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
//        assertNull(predicate);
    }

    @Test
    void testFetchData_WithSortAndFilter() {
        when(request.getSortRequest()).thenReturn(sortRequest);
        when(request.getFilterCriteria()).thenReturn(new ArrayList<>());
        when(request.getPageNo()).thenReturn(1);
        when(request.getPageSize()).thenReturn(10);
        when(sortRequest.getFieldName()).thenReturn("fieldName");
        when(sortRequest.getOrder()).thenReturn("ASC");

        Map<String, RunnerEntityMapping> tableName = new HashMap<>();
        tableName.put("fieldName", runnerEntityMapping);
        when(runnerEntityMapping.getTableName()).thenReturn("ShipmentDetails");

        Pair<Specification<ShipmentDetails>, Pageable> result = DbAccessHelper.fetchData(request, ShipmentDetails.class, tableName);
        assertNotNull(result);
        assertEquals(PageRequest.of(0, 10, Sort.by("fieldName").ascending()), result.getRight());
    }

    @Test
    void fetchDataTableNamesNotNullPredicateIN() {

        List<String> statusList = Arrays.asList("0", "1");

        ListCommonRequest listCommonRequestIn = jsonTestUtility.getListRequestIN();
        listCommonRequestIn.getFilterCriteria().get(0).getInnerFilter().get(0).getCriteria().setValue(statusList);
        listCommonRequestIn.setContainsText("status");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(String.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequestIn, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        assertThrows(RuntimeException.class, () -> {
            specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        });
    }

    @Test
    void fetchDataTableNamesNotNullPredicateDesc() {
        ListCommonRequest listCommonRequest = jsonTestUtility.getListRequest5();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Integer.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
        //when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullAndOperator() {

        ListCommonRequest listCommonRequest6 = jsonTestUtility.getListRequest6();
        listCommonRequest6.setContainsText("status");
        listCommonRequest6.setContainsText("guid");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("shipment_details", RunnerEntityMapping.builder().tableName("shipment_details").build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Integer.class).build());
        tableNames.put("guid", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(UUID.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest6, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

//        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);
////        when(root.getJoins()).thenReturn(Set.of(join));
//        when(root.getFetches()).thenReturn(Collections.emptySet());
//        Predicate mockPredicate = mock(Predicate.class);
//        when(criteriaBuilder.equal(any(), any())).thenReturn(mockPredicate);
//        when(criteriaBuilder.and(any(Predicate.class), any(Predicate.class))).thenReturn(mockPredicate);
//        when(criteriaBuilder.or(any(Predicate.class), any(Predicate.class))).thenReturn(mockPredicate);
//
//        // Mock Path methods
//        when(root.get("status")).thenReturn((Path) path);
//        when(root.get("guid")).thenReturn((Path) path);
//        when(path.as(any())).thenReturn((Path) path);
//
//
//        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
//        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);
//
//        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
//        assertNull(predicate);
    }

    @Test
    void fetchDataGlobalSearchNull() {
        listCommonRequest.setFilterCriteria(Collections.emptyList());
        listCommonRequest.setContainsText("text");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataSortRequestOr() {
        ListCommonRequest listCommonRequest7 = jsonTestUtility.getListRequest7();
        listCommonRequest7.getFilterCriteria().get(1).setLogicOperator("or");
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest7, ShipmentDetails.class));
    }

    @Test
    void fetchDataSortRequestAnd() {
        ListCommonRequest listCommonRequest7 = jsonTestUtility.getListRequest7();
        listCommonRequest7.getFilterCriteria().get(1).setLogicOperator("and");
        assertNotNull(dbAccessHelper.fetchData(listCommonRequest7, ShipmentDetails.class));
    }

    @Test
    void fetchDataTableNamesNotNullPredicateLongClass() {

        Root<ShipmentDetails> root1 = mock(Root.class);
        CriteriaQuery<ShipmentDetails> criteriaQuery = mock(CriteriaQuery.class);
        ListCommonRequest listCommonRequest4 = jsonTestUtility.getListRequest4();
        listCommonRequest4.setIncludeTbls(Arrays.asList("status"));

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").isContainsText(true).dataType(String.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest4, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        Predicate predicate = specification.toPredicate(root1, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullPredicateLongClassDesc() {

        Root<ShipmentDetails> root1 = mock(Root.class);
        CriteriaQuery<ShipmentDetails> criteriaQuery = mock(CriteriaQuery.class);
        ListCommonRequest listCommonRequest8 = jsonTestUtility.getListRequest8();
        listCommonRequest8.setIncludeTbls(Arrays.asList("status"));

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").isContainsText(true).dataType(String.class).build());

        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest8, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        Predicate predicate = specification.toPredicate(root1, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullPredicateGetEnum() {
        ListCommonRequest listCommonRequest9 = jsonTestUtility.getListRequest9();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("andesStatus", RunnerEntityMapping.builder().tableName("shipment_additional_details").isContainsText(true).dataType(AndesStatus.class).build());

        Pair<Specification<AdditionalDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest9, AdditionalDetails.class, tableNames);
        Specification<AdditionalDetails> specification = pair.getLeft();
        assertNotNull(specification);
        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join("shipment_additional_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate((Root<AdditionalDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesLocalDateTimeGreaterOtherFormat() {
        ListCommonRequest listCommonRequest10 = jsonTestUtility.getListRequest10();
        listCommonRequest10.getFilterCriteria().get(0).getInnerFilter().get(1).getCriteria()
                .setValue(LocalDateTime.now());

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Long.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest10, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
        assertNotNull(pair.getRight()); // Just verify the specification and pageable were returned
    }

    @Test
    void fetchDataTableNamesLocalDateTimeGreaterOtherFormatLesser() {
        ListCommonRequest listCommonRequest11 = jsonTestUtility.getListRequest11();
        listCommonRequest11.getFilterCriteria().get(0).getInnerFilter().get(1).getCriteria()
                .setValue(LocalDateTime.now());

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("eta", RunnerEntityMapping.builder()
                .tableName("shipment_details").isContainsText(true)
                .dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder()
                .tableName("shipment_details").isContainsText(true)
                .dataType(LocalDateTime.class).build());
        tableNames.put("status", RunnerEntityMapping.builder()
                .tableName("shipment_details").isContainsText(true)
                .dataType(Long.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair =
                dbAccessHelper.fetchData(listCommonRequest11, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        // Setup mocks for toPredicate execution
//        setupMocksForPredicateExecution(tableNames);
//
//        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
//        assertNotNull(predicate);
    }

    @Test
    void fetchDataTableNamesLocalDateTimeGreaterOtherFormatLesserEqual() {
        ListCommonRequest listCommonRequest12 = jsonTestUtility.getListRequest12();
        listCommonRequest12.getFilterCriteria().get(0).getInnerFilter().get(1).getCriteria().setValue(LocalDateTime.now());

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Long.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest12, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
//        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
//        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);
//
//        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
//        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesLocalDateTimeGreaterOtherFormatGreaterEqual() {
        ListCommonRequest listCommonRequest13 = jsonTestUtility.getListRequest13();
        listCommonRequest13.getFilterCriteria().get(0).getInnerFilter().get(1).getCriteria().setValue(LocalDateTime.now());

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Long.class).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest13, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);
//        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
//        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);
//
//        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
//        assertNull(predicate);
    }

    @Test
    void fetchDataForNestedCriteriaFieldName() {
        Criteria fakeCriteria = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("1").build();
        List<FilterCriteria> innerFilterCriteria = new ArrayList<>();
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria).build());

        List<FilterCriteria> outerFilterCriteria = new ArrayList<>();
        outerFilterCriteria.add(FilterCriteria.builder().innerFilter(innerFilterCriteria).build());
        ListCommonRequest fakeListCommonRequest = ListCommonRequest.builder().filterCriteria(outerFilterCriteria).build();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("pickupTransporterOrgCode", RunnerEntityMapping.builder().parentTable(Constants.PICKUP_DETAILS).tableName("transportDetails").dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(fakeListCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join("pickupDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(join.join("transportDetails", JoinType.LEFT)).thenReturn((Join) join);
        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataForNestedCriteriaFieldName_rootGetFetchNull() {
        Criteria fakeCriteria = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("1").build();
        List<FilterCriteria> innerFilterCriteria = new ArrayList<>();
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria).build());

        List<FilterCriteria> outerFilterCriteria = new ArrayList<>();
        outerFilterCriteria.add(FilterCriteria.builder().innerFilter(innerFilterCriteria).build());
        ListCommonRequest fakeListCommonRequest = ListCommonRequest.builder().filterCriteria(outerFilterCriteria).build();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("pickupTransporterOrgCode", RunnerEntityMapping.builder().parentTable(Constants.PICKUP_DETAILS).tableName("transportDetails").dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(fakeListCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join("pickupDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(join.join("transportDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(root.getJoins()).thenReturn(null);
        when(root.getFetches()).thenReturn(null);
        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataForNestedCriteriaFieldName_rootGetJoinNotEmpty() {
        Criteria fakeCriteria = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("1").build();
        List<FilterCriteria> innerFilterCriteria = new ArrayList<>();
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria).build());

        List<FilterCriteria> outerFilterCriteria = new ArrayList<>();
        outerFilterCriteria.add(FilterCriteria.builder().innerFilter(innerFilterCriteria).build());
        ListCommonRequest fakeListCommonRequest = ListCommonRequest.builder().filterCriteria(outerFilterCriteria).build();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("pickupTransporterOrgCode", RunnerEntityMapping.builder().parentTable(Constants.PICKUP_DETAILS).tableName("transportDetails").dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(fakeListCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join("pickupDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(join.join("transportDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(root.getJoins()).thenReturn(Set.of(mock(Join.class)));
        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataForNestedCriteriaFieldName_rootGetFetchesNotEmpty() {
        Criteria fakeCriteria = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("1").build();
        List<FilterCriteria> innerFilterCriteria = new ArrayList<>();
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria).build());

        List<FilterCriteria> outerFilterCriteria = new ArrayList<>();
        outerFilterCriteria.add(FilterCriteria.builder().innerFilter(innerFilterCriteria).build());
        ListCommonRequest fakeListCommonRequest = ListCommonRequest.builder().filterCriteria(outerFilterCriteria).build();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("pickupTransporterOrgCode", RunnerEntityMapping.builder().parentTable(Constants.PICKUP_DETAILS).tableName("transportDetails").dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(fakeListCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join("pickupDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(join.join("transportDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(root.getFetches()).thenReturn(Set.of(mock(Fetch.class)));
        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataForNestedCriteriaFieldName_mapKeyExist() {
        Criteria fakeCriteria1 = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("1").build();
        Criteria fakeCriteria2 = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("2").build();
        List<FilterCriteria> innerFilterCriteria = new ArrayList<>();
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria1).build());
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria2).logicOperator("OR").build());

        List<FilterCriteria> outerFilterCriteria = new ArrayList<>();
        outerFilterCriteria.add(FilterCriteria.builder().innerFilter(innerFilterCriteria).build());
        ListCommonRequest fakeListCommonRequest = ListCommonRequest.builder().filterCriteria(outerFilterCriteria).build();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("pickupTransporterOrgCode", RunnerEntityMapping.builder().parentTable(Constants.PICKUP_DETAILS).tableName("transportDetails").dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(fakeListCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join("pickupDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(join.join("transportDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(root.getFetches()).thenReturn(Set.of(mock(Fetch.class)));
        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataForNestedCriteriaFieldName_mapKeyExistInRootJoin() {
        Criteria fakeCriteria1 = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("1").build();
        Criteria fakeCriteria2 = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("2").build();
        List<FilterCriteria> innerFilterCriteria = new ArrayList<>();
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria1).build());
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria2).logicOperator("OR").build());

        List<FilterCriteria> outerFilterCriteria = new ArrayList<>();
        outerFilterCriteria.add(FilterCriteria.builder().innerFilter(innerFilterCriteria).build());
        ListCommonRequest fakeListCommonRequest = ListCommonRequest.builder().filterCriteria(outerFilterCriteria).build();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("pickupTransporterOrgCode", RunnerEntityMapping.builder().parentTable(Constants.PICKUP_DETAILS).tableName("transportDetails").dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(fakeListCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join("pickupDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(join.join("transportDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(root.getJoins()).thenReturn((Set) Set.of(join));
        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataForNestedCriteriaFieldName_mapKeyExistInRootFetches() {
        Criteria fakeCriteria1 = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("1").build();
        Criteria fakeCriteria2 = Criteria.builder().fieldName("pickupTransporterOrgCode").operator("=").value("2").build();
        List<FilterCriteria> innerFilterCriteria = new ArrayList<>();
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria1).build());
        innerFilterCriteria.add(FilterCriteria.builder().criteria(fakeCriteria2).logicOperator("OR").build());

        List<FilterCriteria> outerFilterCriteria = new ArrayList<>();
        outerFilterCriteria.add(FilterCriteria.builder().innerFilter(innerFilterCriteria).build());
        ListCommonRequest fakeListCommonRequest = ListCommonRequest.builder().filterCriteria(outerFilterCriteria).build();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("pickupTransporterOrgCode", RunnerEntityMapping.builder().parentTable(Constants.PICKUP_DETAILS).tableName("transportDetails").dataType(String.class).fieldName(Constants.ORG_CODE).isContainsText(true).build());

        Pair<Specification<ShipmentDetails>, Pageable> pair = dbAccessHelper.fetchData(fakeListCommonRequest, ShipmentDetails.class, tableNames);
        Specification<ShipmentDetails> specification = pair.getLeft();
        assertNotNull(specification);

        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);
        when(root.join("pickupDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(join.join("transportDetails", JoinType.LEFT)).thenReturn((Join) join);
        when(root.getFetches()).thenReturn((Set) Set.of(join));
        Predicate predicate = specification.toPredicate((Root<ShipmentDetails>) root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    // Helper method to setup all necessary mocks for predicate execution
    @SuppressWarnings("unchecked")
    private void setupMocksForPredicateExecution(Map<String, RunnerEntityMapping> tableNames) {
        // Setup result type
        when(criteriaQuery.getResultType()).thenReturn(ShipmentDetails.class);

        // Create join mock - this is what root.join() and root.fetch() should return
        Join mockJoinFetch = mock(Join.class);
        Fetch mockFetch = mock(Fetch.class);

        // Setup joins - returns the join mock
        when(root.join(anyString(), any(JoinType.class))).thenReturn((Join) mockJoinFetch);

        // Setup fetches - cast to Join since Fetch extends From which is similar
        when(root.fetch(anyString(), any(JoinType.class))).thenReturn((Fetch) mockFetch);

        // Setup collections to return empty sets to avoid conflicts
        when(root.getJoins()).thenReturn(Collections.emptySet());
        when(root.getFetches()).thenReturn(Collections.emptySet());

        // Setup query order list
        when(criteriaQuery.getOrderList()).thenReturn(Collections.emptyList());
        when(criteriaQuery.distinct(anyBoolean())).thenReturn(criteriaQuery);

        // Setup path operations on root
        when(root.get(anyString())).thenReturn((Path) path);

        // Setup path operations on join/fetch mock
        when(mockJoinFetch.get(anyString())).thenReturn((Path) path);

        when(path.as(any())).thenReturn((Path) path);

        // Setup comparison predicates
        Predicate mockPredicate = mock(Predicate.class);
        when(criteriaBuilder.greaterThan(any(Path.class), any(Comparable.class)))
                .thenReturn(mockPredicate);
        when(criteriaBuilder.lessThan(any(Path.class), any(Comparable.class)))
                .thenReturn(mockPredicate);
        when(criteriaBuilder.greaterThanOrEqualTo(any(Path.class), any(Comparable.class)))
                .thenReturn(mockPredicate);
        when(criteriaBuilder.lessThanOrEqualTo(any(Path.class), any(Comparable.class)))
                .thenReturn(mockPredicate);
        when(criteriaBuilder.equal(any(Path.class), any()))
                .thenReturn(mockPredicate);
        when(criteriaBuilder.notEqual(any(Path.class), any()))
                .thenReturn(mockPredicate);

        // Setup order operations
        when(criteriaBuilder.asc(any())).thenReturn(mock(Order.class));
        when(criteriaBuilder.desc(any())).thenReturn(mock(Order.class));
    }
}