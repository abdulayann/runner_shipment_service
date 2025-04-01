package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
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
import org.hibernate.query.criteria.internal.CriteriaBuilderImpl;
import org.hibernate.query.criteria.internal.CriteriaQueryImpl;
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

import javax.persistence.criteria.*;
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
    private Root<Object> root;

    @Mock
    private CriteriaQuery<?> query;

    @Mock
    private CriteriaQueryImpl criteriaQuery;

    @Mock
    private CriteriaBuilder criteriaBuilder;

    @Mock
    private CriteriaBuilderImpl criteriaBuilderImpl;

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
        query = new CriteriaQueryImpl<Object>(criteriaBuilderImpl, Object.class);
        criteriaQuery = new CriteriaQueryImpl<Object>(criteriaBuilderImpl, Object.class);
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


        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
        CriteriaBuilder.In<Object> inClause = mock(CriteriaBuilder.In.class);
        when(root.join(Constants.CARRIER_DETAILS, JoinType.LEFT)).thenReturn((Join) join);
        when(criteriaBuilder.in(path.get("id"))).thenReturn(inClause);
        when(criteriaBuilder.in(path.get("guid"))).thenReturn(inClause);
        when(criteriaBuilder.in(path.get("customerCategory"))).thenReturn(inClause);
        when(criteriaBuilder.in(path.get("eta"))).thenReturn(inClause);
        when(inClause.value(anyList())).thenReturn(inClause);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
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

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest1, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
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

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest1, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullPredicateLocalDateTimeIsEnum() {
        listCommonRequestIsEnum.setContainsText("status");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Enum.class).build());
        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequestIsEnum, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
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

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequestIn, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        assertThrows(RuntimeException.class, () -> {
            specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        });
    }

    @Test
    void fetchDataTableNamesNotNullPredicateDesc() {
        ListCommonRequest listCommonRequest = jsonTestUtility.getListRequest5();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Integer.class).build());

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        //when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullAndOperator() {

        ListCommonRequest listCommonRequest6 = jsonTestUtility.getListRequest6();
        listCommonRequest6.setContainsText("status");
        listCommonRequest6.setContainsText("guid");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Integer.class).build());
        tableNames.put("guid", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(UUID.class).build());

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest6, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataGlobalSearchNull() {
        listCommonRequest.setFilterCriteria(Collections.emptyList());
        listCommonRequest.setContainsText("text");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
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

        Root<String> root1 = mock(Root.class);
        CriteriaQuery criteriaQuery = new CriteriaQueryImpl<String>(criteriaBuilderImpl, String.class);
        ListCommonRequest listCommonRequest4 = jsonTestUtility.getListRequest4();
        listCommonRequest4.setIncludeTbls(Arrays.asList("status"));

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").isContainsText(true).dataType(String.class).build());

        Pair<Specification<String>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest4, ShipmentDetails.class, tableNames);
        Specification<String> specification = pair.getLeft();
        assertNotNull(specification);

        Predicate predicate = specification.toPredicate(root1, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullPredicateLongClassDesc() {

        Root<String> root1 = mock(Root.class);
        CriteriaQuery criteriaQuery = new CriteriaQueryImpl<String>(criteriaBuilderImpl, String.class);
        ListCommonRequest listCommonRequest8 = jsonTestUtility.getListRequest8();
        listCommonRequest8.setIncludeTbls(Arrays.asList("status"));

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("status", RunnerEntityMapping.builder().tableName("ShipmentDetails").isContainsText(true).dataType(String.class).build());

        Pair<Specification<String>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest8, ShipmentDetails.class, tableNames);
        Specification<String> specification = pair.getLeft();
        assertNotNull(specification);

        Predicate predicate = specification.toPredicate(root1, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesNotNullPredicateGetEnum() {
       ListCommonRequest listCommonRequest9 = jsonTestUtility.getListRequest9();

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("andesStatus", RunnerEntityMapping.builder().tableName("shipment_additional_details").isContainsText(true).dataType(AndesStatus.class).build());

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest9, AdditionalDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_additional_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesLocalDateTimeGreaterOtherFormat () {
        ListCommonRequest listCommonRequest10 = jsonTestUtility.getListRequest10();
        listCommonRequest10.getFilterCriteria().get(0).getInnerFilter().get(1).getCriteria().setValue(LocalDateTime.now());

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Long.class).build());

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest10, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesLocalDateTimeGreaterOtherFormatLesser () {
        ListCommonRequest listCommonRequest11 = jsonTestUtility.getListRequest11();
        listCommonRequest11.getFilterCriteria().get(0).getInnerFilter().get(1).getCriteria().setValue(LocalDateTime.now());

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Long.class).build());

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest11, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesLocalDateTimeGreaterOtherFormatLesserEqual () {
        ListCommonRequest listCommonRequest12 = jsonTestUtility.getListRequest12();
        listCommonRequest12.getFilterCriteria().get(0).getInnerFilter().get(1).getCriteria().setValue(LocalDateTime.now());

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Long.class).build());

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest12, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }

    @Test
    void fetchDataTableNamesLocalDateTimeGreaterOtherFormatGreaterEqual () {
        ListCommonRequest listCommonRequest13 = jsonTestUtility.getListRequest13();
        listCommonRequest13.getFilterCriteria().get(0).getInnerFilter().get(1).getCriteria().setValue(LocalDateTime.now());

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(LocalDateTime.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(Long.class).build());

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest13, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

        Predicate predicate = specification.toPredicate(root, criteriaQuery, criteriaBuilder);
        assertNull(predicate);
    }
}
