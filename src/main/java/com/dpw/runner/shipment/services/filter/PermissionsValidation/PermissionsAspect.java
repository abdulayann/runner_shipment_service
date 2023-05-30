package com.dpw.runner.shipment.services.filter.PermissionsValidation;



import com.dpw.runner.shipment.services.dto.request.Criteria;
import com.dpw.runner.shipment.services.dto.request.FilterCriteria;
import com.dpw.runner.shipment.services.dto.request.Pageable;
import org.apache.poi.ss.formula.functions.T;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.*;

@Aspect
@Component
public class PermissionsAspect {

    @Autowired
    private PermissionsContext permissionsContext;


    @Autowired
    private EntityManager entityManager;
    @Before("execution(* com.dpw.runner.shipment.services.service.IShipmentService+.*(..)) && args(pageable)")
    public void beforeFindOfMultiTenancyRepository(JoinPoint joinPoint, Pageable pageable) {
        List<FilterCriteria> criterias = new ArrayList<>();
        if(permissionsContext.getPermissions().contains("airexportfclshipmentList")) {
            List<FilterCriteria> innerFilters = new ArrayList();

            innerFilters.add(constructCriteria("transportMode", "AIR","=" , null));
            innerFilters.add(constructCriteria("direction", "EXP","=" , "and"));
            innerFilters.add(constructCriteria("shipmentType", "FCL","=" , "and"));
            criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        }
        if(permissionsContext.getPermissions().contains("airexportlclshipmentList")) {
            List<FilterCriteria> innerFilters = new ArrayList();
            innerFilters.add(constructCriteria("transportMode", "AIR","=" , null));
            innerFilters.add(constructCriteria("direction", "EXP","=" , "and"));
            innerFilters.add(constructCriteria("shipmentType", "LCL","=" , "and"));
            criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        }
        if(permissionsContext.getPermissions().contains("airimportfclshipmentlist")) {
            List<FilterCriteria> innerFilters = new ArrayList();
            innerFilters.add(constructCriteria("transportMode", "AIR","=" , null));
            innerFilters.add(constructCriteria("direction", "IMP","=" , "and"));
            innerFilters.add(constructCriteria("shipmentType", "FCL","=" , "and"));
            criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        }
        if(permissionsContext.getPermissions().contains("airimportlclshipmentlist")) {
            List<FilterCriteria> innerFilters = new ArrayList();
            innerFilters.add(constructCriteria("transportMode", "AIR","=" , null));
            innerFilters.add(constructCriteria("direction", "IMP","=" , "and"));
            innerFilters.add(constructCriteria("shipmentType", "LCL","=" , "and"));
            criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        }
        if(permissionsContext.getPermissions().contains("seaexportfclshipmentList")) {
            List<FilterCriteria> innerFilters = new ArrayList();
            innerFilters.add(constructCriteria("transportMode", "SEA","=" , null));
            innerFilters.add(constructCriteria("direction", "EXP","=" , "and"));
            innerFilters.add(constructCriteria("shipmentType", "FCL","=" , "and"));
            criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        }
        if(permissionsContext.getPermissions().contains("seaexportlclshipmentList")) {
            List<FilterCriteria> innerFilters = new ArrayList();
            innerFilters.add(constructCriteria("transportMode", "SEA","=" , null));
            innerFilters.add(constructCriteria("direction", "EXP","=" , "and"));
            innerFilters.add(constructCriteria("shipmentType", "LCL","=" , "and"));
            criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        }
        if(permissionsContext.getPermissions().contains("seaimportfclshipmentlist")) {
            List<FilterCriteria> innerFilters = new ArrayList();
            innerFilters.add(constructCriteria("transportMode", "SEA","=" , null));
            innerFilters.add(constructCriteria("direction", "IMP","=" , "and"));
            innerFilters.add(constructCriteria("shipmentType", "FCL","=" , "and"));
            criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        }
        if(permissionsContext.getPermissions().contains("seaimportlclshipmentlist")) {
            List<FilterCriteria> innerFilters = new ArrayList();
            innerFilters.add(constructCriteria("transportMode", "SEA","=" , null));
            innerFilters.add(constructCriteria("direction", "IMP","=" , "and"));
            innerFilters.add(constructCriteria("shipmentType", "LCL","=" , "and"));
            criterias.add(FilterCriteria.builder().innerFilter(innerFilters).logicOperator(criterias.isEmpty() ? null : "or").build());
        }

        FilterCriteria criteria1 = FilterCriteria.builder().innerFilter(pageable.getFilterCriteria()).build();
        FilterCriteria criteria2 = FilterCriteria.builder().innerFilter(criterias).logicOperator("AND").build();
        pageable.setFilterCriteria(Arrays.asList(criteria1, criteria2));
    }

    private FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }
}
