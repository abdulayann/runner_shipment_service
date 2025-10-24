package com.dpw.runner.shipment.services.repository;

import com.dpw.runner.shipment.services.dto.request.Criteria;
import com.dpw.runner.shipment.services.dto.request.FilterCriteria;
import com.dpw.runner.shipment.services.dto.request.SortRequest;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.jpa.domain.Specification;

import javax.persistence.criteria.*;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class IShipmentDaoTest {

    @Test
    public void testCreateSpecification_AssignedTo_Equals() {
        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator("=")
                .value(123)
                .build();

        SortRequest sortRequest = SortRequest.builder()
                .fieldName("houseBill")
                .order("ASC")
                .build();

        Map<String, Join<Class, ShipmentDetails>> map = new HashMap<>();

        Specification<ShipmentDetails> spec = IShipmentDao.createSpecification(criteria, sortRequest, map);

        assertNotNull(spec);
    }

    @Test
    public void testCreateSpecification_AssignedTo_NotEquals() {
        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator("!=")
                .value(456)
                .build();

        SortRequest sortRequest = null;
        Map<String, Join<Class, ShipmentDetails>> map = new HashMap<>();

        Specification<ShipmentDetails> spec = IShipmentDao.createSpecification(criteria, sortRequest, map);

        assertNotNull(spec);
    }

    @Test
    public void testCreateSpecification_AssignedTo_GreaterThan() {
        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator(">")
                .value(100)
                .build();

        Map<String, Join<Class, ShipmentDetails>> map = new HashMap<>();

        Specification<ShipmentDetails> spec = IShipmentDao.createSpecification(criteria, null, map);

        assertNotNull(spec);
    }

    @Test
    public void testCreateSpecification_AssignedTo_LessThan() {
        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator("<")
                .value(500)
                .build();

        Map<String, Join<Class, ShipmentDetails>> map = new HashMap<>();

        Specification<ShipmentDetails> spec = IShipmentDao.createSpecification(criteria, null, map);

        assertNotNull(spec);
    }

    @Test
    public void testCreateSpecification_AssignedTo_In() {
        List<Integer> assignedToValues = Arrays.asList(100, 200, 300);

        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator("IN")
                .value(assignedToValues)
                .build();

        Map<String, Join<Class, ShipmentDetails>> map = new HashMap<>();

        Specification<ShipmentDetails> spec = IShipmentDao.createSpecification(criteria, null, map);

        assertNotNull(spec);
    }

    @Test
    public void testCreateSpecificationWithPredicate_AssignedTo_Equals() {
        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator("=")
                .value(123)
                .build();

        CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
        Path path = mock(Path.class);

        when(path.get("assignedTo")).thenReturn(path);
        when(criteriaBuilder.equal(any(), any())).thenReturn(mock(Predicate.class));

        Predicate predicate = IShipmentDao.createSpecification(Integer.class, criteria, path, criteriaBuilder);

        assertNotNull(predicate);
        verify(criteriaBuilder, times(1)).equal(path, 123);
    }

    @Test
    public void testCreateSpecificationWithPredicate_AssignedTo_NotEquals() {
        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator("!=")
                .value(456)
                .build();

        CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
        Path path = mock(Path.class);

        when(path.get("assignedTo")).thenReturn(path);
        when(criteriaBuilder.notEqual(any(), any())).thenReturn(mock(Predicate.class));

        Predicate predicate = IShipmentDao.createSpecification(Integer.class, criteria, path, criteriaBuilder);

        assertNotNull(predicate);
        verify(criteriaBuilder, times(1)).notEqual(path, 456);
    }

    @Test
    public void testCreateSpecificationWithPredicate_AssignedTo_GreaterThan() {
        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator(">")
                .value(100)
                .build();

        CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
        Path path = mock(Path.class);

        when(path.get("assignedTo")).thenReturn(path);
        when(criteriaBuilder.gt(any(), any())).thenReturn(mock(Predicate.class));

        Predicate predicate = IShipmentDao.createSpecification(Integer.class, criteria, path, criteriaBuilder);

        assertNotNull(predicate);
        verify(criteriaBuilder, times(1)).gt(path, 100);
    }

    @Test
    public void testCreateSpecificationWithPredicate_AssignedTo_LessThan() {
        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator("<")
                .value(500)
                .build();

        CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
        Path path = mock(Path.class);

        when(path.get("assignedTo")).thenReturn(path);
        when(criteriaBuilder.lt(any(), any())).thenReturn(mock(Predicate.class));

        Predicate predicate = IShipmentDao.createSpecification(Integer.class, criteria, path, criteriaBuilder);

        assertNotNull(predicate);
        verify(criteriaBuilder, times(1)).lt(path, 500);
    }

    @Test
    public void testCreateSpecificationWithPredicate_AssignedTo_In() {
        List<Integer> assignedToValues = Arrays.asList(100, 200, 300);

        Criteria criteria = Criteria.builder()
                .fieldName("assignedTo")
                .operator("IN")
                .value(assignedToValues)
                .build();

        CriteriaBuilder criteriaBuilder = mock(CriteriaBuilder.class);
        Path path = mock(Path.class);
        CriteriaBuilder.In inPredicate = mock(CriteriaBuilder.In.class);

        when(path.get("assignedTo")).thenReturn(path);
        when(criteriaBuilder.in(any())).thenReturn(inPredicate);
        when(inPredicate.value(any())).thenReturn(inPredicate);

        Predicate predicate = IShipmentDao.createSpecification(Integer.class, criteria, path, criteriaBuilder);

        assertNotNull(predicate);
        verify(criteriaBuilder, times(1)).in(path);
        verify(inPredicate, times(1)).value(assignedToValues);
    }

    @Test
    public void testFetchShipmentData_WithAssignedToFilter() {
        FilterCriteria filterCriteria = FilterCriteria.builder()
                .innerFilter(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("assignedTo")
                                        .operator("=")
                                        .value(123)
                                        .build())
                                .build()
                ))
                .build();

        SortRequest sortRequest = SortRequest.builder()
                .fieldName("houseBill")
                .order("DESC")
                .build();

        Specification<ShipmentDetails> spec = IShipmentDao.fetchShipmentData(
                Arrays.asList(filterCriteria), sortRequest);

        assertNotNull(spec);
    }

    @Test
    public void testFetchShipmentData_WithAssignedToFilter_OrCondition() {
        FilterCriteria filterCriteria1 = FilterCriteria.builder()
                .innerFilter(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("assignedTo")
                                        .operator("=")
                                        .value(123)
                                        .build())
                                .build()
                ))
                .build();

        FilterCriteria filterCriteria2 = FilterCriteria.builder()
                .logicOperator("OR")
                .innerFilter(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("assignedTo")
                                        .operator("=")
                                        .value(456)
                                        .build())
                                .build()
                ))
                .build();

        SortRequest sortRequest = SortRequest.builder()
                .fieldName("houseBill")
                .order("ASC")
                .build();

        Specification<ShipmentDetails> spec = IShipmentDao.fetchShipmentData(
                Arrays.asList(filterCriteria1, filterCriteria2), sortRequest);

        assertNotNull(spec);
    }

    @Test
    public void testFetchShipmentData_WithAssignedToFilter_AndCondition() {
        FilterCriteria filterCriteria1 = FilterCriteria.builder()
                .innerFilter(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("assignedTo")
                                        .operator(">")
                                        .value(100)
                                        .build())
                                .build()
                ))
                .build();

        FilterCriteria filterCriteria2 = FilterCriteria.builder()
                .logicOperator("AND")
                .innerFilter(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("assignedTo")
                                        .operator("<")
                                        .value(500)
                                        .build())
                                .build()
                ))
                .build();

        SortRequest sortRequest = SortRequest.builder()
                .fieldName("houseBill")
                .order("DESC")
                .build();

        Specification<ShipmentDetails> spec = IShipmentDao.fetchShipmentData(
                Arrays.asList(filterCriteria1, filterCriteria2), sortRequest);

        assertNotNull(spec);
    }

    @Test
    public void testGetSpecificationFromFilters_WithAssignedTo() {
        List<FilterCriteria> filters = Arrays.asList(
                FilterCriteria.builder()
                        .criteria(Criteria.builder()
                                .fieldName("assignedTo")
                                .operator("=")
                                .value(789)
                                .build())
                        .build()
        );

        SortRequest sortRequest = SortRequest.builder()
                .fieldName("houseBill")
                .order("ASC")
                .build();

        Map<String, Join<Class, ShipmentDetails>> map = new HashMap<>();

        Specification<ShipmentDetails> spec = IShipmentDao.getSpecificationFromFilters(filters, sortRequest, map);

        assertNotNull(spec);
    }

    @Test
    public void testGetSpecificationFromFilters_WithNestedAssignedToFilters() {
        FilterCriteria nestedFilter = FilterCriteria.builder()
                .innerFilter(Arrays.asList(
                        FilterCriteria.builder()
                                .criteria(Criteria.builder()
                                        .fieldName("assignedTo")
                                        .operator("=")
                                        .value(111)
                                        .build())
                                .build(),
                        FilterCriteria.builder()
                                .logicOperator("OR")
                                .criteria(Criteria.builder()
                                        .fieldName("assignedTo")
                                        .operator("=")
                                        .value(222)
                                        .build())
                                .build()
                ))
                .build();

        SortRequest sortRequest = null;
        Map<String, Join<Class, ShipmentDetails>> map = new HashMap<>();

        Specification<ShipmentDetails> spec = IShipmentDao.getSpecificationFromFilters(
                Arrays.asList(nestedFilter), sortRequest, map);

        assertNotNull(spec);
    }

    @Test
    public void testTableNames_ContainsAssignedTo() {
        assertTrue(IShipmentDao.tableNames.containsKey("assignedTo"));
        assertEquals("ShipmentDetails", IShipmentDao.tableNames.get("assignedTo").getTableName());
        assertEquals(Integer.class, IShipmentDao.tableNames.get("assignedTo").getDataType());
    }
}
