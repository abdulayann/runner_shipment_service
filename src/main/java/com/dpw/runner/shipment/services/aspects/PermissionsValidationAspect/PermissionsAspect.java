package com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect;


import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.requests.FilterCriteria;
import com.dpw.runner.shipment.services.commons.requests.ListCommonRequest;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.utils.ExcludePermissions;
import com.dpw.runner.shipment.services.utils.PermissionUtil;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

import static com.dpw.runner.shipment.services.commons.constants.Constants.CONSOLIDATION_LIST_PERMISSION;
import static com.dpw.runner.shipment.services.commons.constants.Constants.SHIPMENT_LIST_PERMISSION;

@Aspect
@Component
public class PermissionsAspect {

    @Before("execution(* com.dpw.runner.shipment.services.service.interfaces.IShipmentService+.*(..)) && args(commonRequestModel, getMasterData)")
    public void beforeFindOfMultiTenancyRepository(JoinPoint joinPoint, CommonRequestModel commonRequestModel, boolean getMasterData) throws RunnerException {
        if (commonRequestModel.getData() == null || !commonRequestModel.getData().getClass().isAssignableFrom(ListCommonRequest.class) || checkExcludePermissionsFilter(joinPoint)) {
            return;
        }

        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        List<String> permissionList = PermissionsContext.getPermissions(SHIPMENT_LIST_PERMISSION);
        if(permissionList == null || permissionList.isEmpty())
            throw new RunnerException("Unable to list shipments due to insufficient list permissions");
        permissionList.sort(new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return Integer.compare(o1.length(), o2.length());
            }
        });
        List<FilterCriteria> criterias = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, true);

        FilterCriteria criteria1 = null;
        if(listCommonRequest.getFilterCriteria() != null && !listCommonRequest.getFilterCriteria().isEmpty()) {
           criteria1 = FilterCriteria.builder().innerFilter(listCommonRequest.getFilterCriteria()).build();
        }
        FilterCriteria criteria2 = FilterCriteria.builder().innerFilter(criterias).build();
        if(criteria2 != null && (criteria2.getCriteria() != null || (criteria2.getInnerFilter() != null && !criteria2.getInnerFilter().isEmpty()))) {
            if (criteria1 != null && !criteria1.getInnerFilter().isEmpty()) {
                criteria2.setLogicOperator("AND");
                listCommonRequest.setFilterCriteria(Arrays.asList(criteria1, criteria2));
            } else
                listCommonRequest.setFilterCriteria(List.of(criteria2));
        }
    }

    @Before("execution(* com.dpw.runner.shipment.services.service.interfaces.IConsolidationService+.*(..)) && args(commonRequestModel, getMasterData)")
    public void beforeConsolidationList(JoinPoint joinPoint, CommonRequestModel commonRequestModel, boolean getMasterData) throws RunnerException {
        if (commonRequestModel.getData() == null || !commonRequestModel.getData().getClass().isAssignableFrom(ListCommonRequest.class)) {
            return;
        }
        ListCommonRequest listCommonRequest = (ListCommonRequest) commonRequestModel.getData();
        List<String> permissionList = PermissionsContext.getPermissions(CONSOLIDATION_LIST_PERMISSION);
        if(permissionList == null || permissionList.size() == 0)
            throw new RunnerException("Unable to list consolidations due to insufficient list permissions.");
        permissionList.sort(new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return Integer.compare(o1.length(), o2.length());
            }
        });
        List<FilterCriteria> criterias = PermissionUtil.generateFilterCriteriaFromPermissions(permissionList, false);

        FilterCriteria criteria1 = null;
        if(listCommonRequest.getFilterCriteria() != null && listCommonRequest.getFilterCriteria().size() > 0) {
            criteria1 = FilterCriteria.builder().innerFilter(listCommonRequest.getFilterCriteria()).build();
        }
        FilterCriteria criteria2 = FilterCriteria.builder().innerFilter(criterias).build();
        if(criteria2 != null && (criteria2.getCriteria() != null || (criteria2.getInnerFilter() != null && criteria2.getInnerFilter().size() > 0))) {
            if (criteria1 != null && criteria1.getInnerFilter().size() > 0) {
                criteria2.setLogicOperator("AND");
                listCommonRequest.setFilterCriteria(Arrays.asList(criteria1, criteria2));
            } else
                listCommonRequest.setFilterCriteria(Arrays.asList(criteria2));
        }
    }

    private boolean checkExcludePermissionsFilter(JoinPoint joinPoint) {
        if(joinPoint != null)
        {
            MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
            if(!Objects.isNull(methodSignature))
            {
                Method method = methodSignature.getMethod();
                return method.isAnnotationPresent(ExcludePermissions.class);
            }
        }
        return false;
    }
}
