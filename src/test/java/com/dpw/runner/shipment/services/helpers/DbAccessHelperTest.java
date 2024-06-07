package com.dpw.runner.shipment.services.helpers;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.commons.requests.RunnerEntityMapping;
import com.dpw.runner.shipment.services.commons.requests.SortRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.nimbusds.jose.util.Pair;
import org.hibernate.internal.SessionFactoryImpl;
import org.hibernate.query.criteria.internal.CriteriaBuilderImpl;
import org.hibernate.query.criteria.internal.CriteriaQueryImpl;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

import static org.codehaus.groovy.runtime.DefaultGroovyMethods.any;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class DbAccessHelperTest {

    @InjectMocks
    private DbAccessHelper dbAccessHelper;
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
    private Join<?, ?> join;

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
    void fetchDataTableNamesNotNullPredicate() {
        listCommonRequest.setContainsText("transportMode");
        listCommonRequest.setContainsText("status");
        listCommonRequest.setContainsText("eta");
        listCommonRequest.setContainsText("etd");
        listCommonRequest.setContainsText("masterBill");
        listCommonRequest.setContainsText("origin");
        listCommonRequest.setContainsText("destination");

        Map<String, RunnerEntityMapping> tableNames = new HashMap<>();
        tableNames.put("transportMode", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(String.class).build());
        tableNames.put("status", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(String.class).build());
        tableNames.put("eta", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(String.class).build());
        tableNames.put("etd", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(String.class).build());
        tableNames.put("masterBill", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(List.class).build());
        tableNames.put("origin", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(String.class).build());
        tableNames.put("destination", RunnerEntityMapping.builder().tableName("shipment_details").isContainsText(true).dataType(String.class).build());

        Pair<Specification<Object>, Pageable> pair = dbAccessHelper.fetchData(listCommonRequest, ShipmentDetails.class, tableNames);
        Specification<Object> specification = pair.getLeft();
        assertNotNull(specification);
        when(root.join("shipment_details", JoinType.LEFT)).thenReturn((Join) join);

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
}
