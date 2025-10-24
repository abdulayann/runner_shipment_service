package com.dpw.runner.shipment.services;

import com.dpw.runner.shipment.services.dto.request.Criteria;
import com.dpw.runner.shipment.services.dto.request.FilterCriteria;
import com.dpw.runner.shipment.services.dto.request.Pageable;
import com.dpw.runner.shipment.services.filter.PermissionsValidation.PermissionsAspect;
import com.dpw.runner.shipment.services.filter.PermissionsValidation.PermissionsContext;
import org.aspectj.lang.JoinPoint;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.*;

class CriteriaTest {

    @Mock
    private PermissionsContext permissionsContext;

    @InjectMocks
    private PermissionsAspect permissionsAspect;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void beforeFindOfMultiTenancyRepository_ShouldAddFilterCriteriaBasedOnPermissions() {
        Pageable pageable = new Pageable();
        pageable.setFilterCriteria(new ArrayList<>());

        // Act
        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        List<FilterCriteria> criterias = new ArrayList<>();
        List<FilterCriteria> innerFilters = new ArrayList();

        innerFilters.add(constructCriteria("transportMode", "AIR","=" , null));
        innerFilters.add(constructCriteria("direction", "EXP","=" , "and"));
        innerFilters.add(constructCriteria("shipmentType", "FCL","=" , "and"));
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());

        innerFilters.clear();
        innerFilters.add(constructCriteria("transportMode", "AIR","=" , null));
        innerFilters.add(constructCriteria("direction", "EXP","=" , "and"));
        innerFilters.add(constructCriteria("shipmentType", "LCL","=" , "and"));
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());

        innerFilters.clear();
        innerFilters.add(constructCriteria("transportMode", "AIR","=" , null));
        innerFilters.add(constructCriteria("direction", "IMP","=" , "and"));
        innerFilters.add(constructCriteria("shipmentType", "FCL","=" , "and"));
        criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());


        FilterCriteria criteria1 = FilterCriteria.builder().innerFilter(pageable.getFilterCriteria()).build();
        FilterCriteria criteria2 = FilterCriteria.builder().innerFilter(criterias).logicOperator("AND").build();

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        //it should populate the pageable and change its criterias
        verify(permissionsContext, times(1)).getPermissions();
    }

    private FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }

    @Test
    void beforeFindOfMultiTenancyRepository_WithAssignedToFilter_ShouldWork() {
        Pageable pageable = new Pageable();
        List<FilterCriteria> filterCriteriaList = new ArrayList<>();

        filterCriteriaList.add(constructCriteria("assignedTo", 123, "=", null));
        pageable.setFilterCriteria(filterCriteriaList);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        verify(permissionsContext, times(1)).getPermissions();
    }

    @Test
    void beforeFindOfMultiTenancyRepository_WithAssignedToFilter_MultipleValues() {
        Pageable pageable = new Pageable();
        List<FilterCriteria> filterCriteriaList = new ArrayList<>();

        List<FilterCriteria> innerFilters = new ArrayList<>();
        innerFilters.add(constructCriteria("assignedTo", 100, "=", null));
        innerFilters.add(constructCriteria("assignedTo", 200, "=", "or"));
        innerFilters.add(constructCriteria("assignedTo", 300, "=", "or"));

        filterCriteriaList.add(FilterCriteria.builder().innerFilter(innerFilters).build());
        pageable.setFilterCriteria(filterCriteriaList);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        verify(permissionsContext, times(1)).getPermissions();
    }

    @Test
    void beforeFindOfMultiTenancyRepository_WithAssignedToAndTransportModeFilter() {
        Pageable pageable = new Pageable();
        List<FilterCriteria> filterCriteriaList = new ArrayList<>();

        List<FilterCriteria> innerFilters = new ArrayList<>();
        innerFilters.add(constructCriteria("transportMode", "AIR", "=", null));
        innerFilters.add(constructCriteria("assignedTo", 456, "=", "and"));

        filterCriteriaList.add(FilterCriteria.builder().innerFilter(innerFilters).build());
        pageable.setFilterCriteria(filterCriteriaList);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        verify(permissionsContext, times(1)).getPermissions();
    }

    @Test
    void beforeFindOfMultiTenancyRepository_WithAssignedToGreaterThanFilter() {
        Pageable pageable = new Pageable();
        List<FilterCriteria> filterCriteriaList = new ArrayList<>();

        filterCriteriaList.add(constructCriteria("assignedTo", 100, ">", null));
        pageable.setFilterCriteria(filterCriteriaList);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        verify(permissionsContext, times(1)).getPermissions();
    }

    @Test
    void beforeFindOfMultiTenancyRepository_WithAssignedToLessThanFilter() {
        Pageable pageable = new Pageable();
        List<FilterCriteria> filterCriteriaList = new ArrayList<>();

        filterCriteriaList.add(constructCriteria("assignedTo", 500, "<", null));
        pageable.setFilterCriteria(filterCriteriaList);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        verify(permissionsContext, times(1)).getPermissions();
    }

    @Test
    void beforeFindOfMultiTenancyRepository_WithAssignedToNotEqualsFilter() {
        Pageable pageable = new Pageable();
        List<FilterCriteria> filterCriteriaList = new ArrayList<>();

        filterCriteriaList.add(constructCriteria("assignedTo", 999, "!=", null));
        pageable.setFilterCriteria(filterCriteriaList);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        verify(permissionsContext, times(1)).getPermissions();
    }

    @Test
    void beforeFindOfMultiTenancyRepository_WithAssignedToInFilter() {
        Pageable pageable = new Pageable();
        List<FilterCriteria> filterCriteriaList = new ArrayList<>();

        List<Integer> assignedToValues = List.of(100, 200, 300, 400);
        filterCriteriaList.add(constructCriteria("assignedTo", assignedToValues, "IN", null));
        pageable.setFilterCriteria(filterCriteriaList);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        verify(permissionsContext, times(1)).getPermissions();
    }

    @Test
    void beforeFindOfMultiTenancyRepository_WithAssignedToRangeFilter() {
        Pageable pageable = new Pageable();
        List<FilterCriteria> filterCriteriaList = new ArrayList<>();

        List<FilterCriteria> innerFilters = new ArrayList<>();
        innerFilters.add(constructCriteria("assignedTo", 100, ">", null));
        innerFilters.add(constructCriteria("assignedTo", 500, "<", "and"));

        filterCriteriaList.add(FilterCriteria.builder().innerFilter(innerFilters).build());
        pageable.setFilterCriteria(filterCriteriaList);

        permissionsAspect.beforeFindOfMultiTenancyRepository(mock(JoinPoint.class), pageable);

        verify(permissionsContext, times(1)).getPermissions();
    }
}