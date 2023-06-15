package com.dpw.runner.shipment.services.filter.PermissionsValidation;


import com.dpw.runner.shipment.services.dto.request.Criteria;
import com.dpw.runner.shipment.services.dto.request.FilterCriteria;
import com.dpw.runner.shipment.services.dto.request.Pageable;
import com.dpw.runner.shipment.services.utility.CommonUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManager;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

@Aspect
@Component
public class PermissionsAspect {

    private final Logger LOG = LoggerFactory.getLogger(PermissionsAspect.class);
    @Autowired
    private PermissionsContext permissionsContext;
    @Autowired
    private EntityManager entityManager;

    @Before("execution(* com.dpw.runner.shipment.services.service.IShipmentService+.*(..)) && args(pageable)")
    public void beforeFindOfMultiTenancyRepository(JoinPoint joinPoint, Pageable pageable) {
        List<String> permissionList = permissionsContext.getPermissions();
        permissionList.sort(new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return Integer.compare(o1.length(), o2.length());
            }
        });
        List<FilterCriteria> criterias = CommonUtils.generateFilterCriteriaFromPermissions(permissionList);

        FilterCriteria criteria1 = FilterCriteria.builder().innerFilter(pageable.getFilterCriteria()).build();
        FilterCriteria criteria2 = FilterCriteria.builder().innerFilter(criterias).logicOperator("AND").build();
        pageable.setFilterCriteria(Arrays.asList(criteria1, criteria2));
    }

    private FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }
}
