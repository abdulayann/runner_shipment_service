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
import java.util.*;

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
        List<FilterCriteria> criterias = new ArrayList<>();
        // Sort permission list to bring the "ALL" permissions in front .
        // This will help to avoid adding criteria for some property when ALL was already there
        permissionsContext.getPermissions().sort(new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return Integer.compare(o1.length(), o2.length());
            }
        });
        HashSet<String> permissionSet = new HashSet<>();

        for(String permission : permissionsContext.getPermissions()) {
            List<FilterCriteria> innerFilters =
                    CommonUtils.generateFilterCriteriaFromPermissionType(permission, permissionSet);
            //TODO Logic to skip the permissions already covered in the ALL part

            if(!innerFilters.isEmpty()) {
                criterias.add(FilterCriteria.builder().innerFilter(innerFilters)
                        .logicOperator(criterias.isEmpty() ? null : "or").build());
            }
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
