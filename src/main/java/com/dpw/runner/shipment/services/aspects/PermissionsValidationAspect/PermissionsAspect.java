package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.Criteria;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.utils.CommonUtils;
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

    @Before("execution(* com.dpw.runner.shipment.services.service.interfaces.IShipmentService+.*(..)) && args(commonRequestModel)")
    public void beforeFindOfMultiTenancyRepository(JoinPoint joinPoint, CommonRequestModel commonRequestModel) {
        if (commonRequestModel.getData() == null || !commonRequestModel.getData().getClass().isAssignableFrom(ListCommonRequest.class)) {
            return;
        }
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        List<String> permissionList = PermissionsContext.getPermissions();
        permissionList.sort(new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return Integer.compare(o1.length(), o2.length());
            }
        });
        List<FilterCriteria> criterias = CommonUtils.generateFilterCriteriaFromPermissions(permissionList);

        FilterCriteria criteria1 = FilterCriteria.builder().innerFilter(listCommonRequest.getFilterCriteria()).build();
        FilterCriteria criteria2 = FilterCriteria.builder().innerFilter(criterias).logicOperator("AND").build();
        listCommonRequest.setFilterCriteria(Arrays.asList(criteria1, criteria2));
    }

    private FilterCriteria constructCriteria(String fieldName, Object value, String operator, String logicalOperator) {
        Criteria criteria = Criteria.builder().fieldName(fieldName).operator(operator).value(value).build();
        return FilterCriteria.builder().criteria(criteria).logicOperator(logicalOperator).build();
    }
}
