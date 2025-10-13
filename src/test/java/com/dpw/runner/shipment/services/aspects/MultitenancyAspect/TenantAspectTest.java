package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.reflect.MethodSignature;
import org.hibernate.Filter;
import org.hibernate.Session;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import javax.persistence.EntityManager;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class TenantAspectTest {

    @Mock EntityManager entityManager;
    @Mock CommonUtils commonUtils;
    @Mock JoinPoint joinPoint;
    @Mock MethodSignature methodSignature; // used for both method + declaring type
    @Mock Session session;
    @Mock Filter filter;

    TenantAspect tenantAspect;

    @BeforeEach
    void setUp() {
        tenantAspect = new TenantAspect();
        setField(tenantAspect, "entityManager", entityManager);
        setField(tenantAspect, "commonUtils", commonUtils);

        // lenient stubs so tests that don't hit every path don't fail with UnnecessaryStubbingException
        lenient().when(entityManager.unwrap(Session.class)).thenReturn(session);
        lenient().when(session.enableFilter(anyString())).thenReturn(filter);

        lenient().when(filter.setParameter(anyString(), any())).thenReturn(filter);
        lenient().when(filter.setParameterList(anyString(), anyCollection())).thenReturn(filter);

        lenient().when(joinPoint.getSignature()).thenReturn(methodSignature);
    }

    // reflection helper
    private static void setField(Object target, String fieldName, Object value) {
        try {
            var field = target.getClass().getDeclaredField(fieldName);
            field.setAccessible(true);
            field.set(target, value);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    // Test helpers
    public static class DummyRepoWithExclude {
        @com.dpw.runner.shipment.services.utils.ExcludeTenantFilter
        public void excludedMethod() {}
    }

    @Test
    void beforeFindOfMultiTenancyRepository_excludeAnnotation_returnsEarly() throws NoSuchMethodException {
        Method excluded = DummyRepoWithExclude.class.getMethod("excludedMethod");
        when(methodSignature.getMethod()).thenReturn(excluded);
        tenantAspect.beforeFindOfMultiTenancyRepository(joinPoint);
        verify(entityManager, never()).unwrap(Session.class);
    }

    @com.dpw.runner.shipment.services.utils.InterBranchEntity
    static class AnnotatedEntity {}

    @Test
    void beforeFindOfMultiTenancyRepository_interBranch_enablesMultiBranchFilter() throws NoSuchMethodException {
        Method someMethod = TenantAspectTest.class.getMethod("dummyNoAnnotation");
        when(methodSignature.getMethod()).thenReturn(someMethod);
        when(methodSignature.getDeclaringType()).thenReturn(AnnotatedEntity.class);

        try (MockedStatic<TenantContext> tctx = Mockito.mockStatic(TenantContext.class);
             MockedStatic<UserContext> uctx = Mockito.mockStatic(UserContext.class)) {
            tctx.when(TenantContext::getCurrentTenant).thenReturn(100); // Integer
            uctx.when(UserContext::getUser).thenReturn(null);

            InterBranchDto dto = new InterBranchDto();
            dto.setCoLoadStation(true);
            dto.setHubTenantIds(List.of(200, 300));
            when(commonUtils.getInterBranchContext()).thenReturn(dto);

            tenantAspect.beforeFindOfMultiTenancyRepository(joinPoint);

            verify(entityManager, atLeastOnce()).unwrap(Session.class);
            verify(session, times(1)).enableFilter(MultiTenancy.MULTI_BRANCH_FILTER_NAME);
            verify(filter, times(1)).setParameterList(eq(MultiTenancy.TENANT_PARAMETER_NAME), anyCollection());
        }
    }

    public void dummyNoAnnotation() { /* used for methodSignature that has no @ExcludeTenantFilter */ }

    @Test
    void beforeFindOfMultiTenancyRepository_noInterBranch_enablesTenantFilter() throws NoSuchMethodException {
        Method m = TenantAspectTest.class.getMethod("dummyNoAnnotation");
        when(methodSignature.getMethod()).thenReturn(m);
        when(methodSignature.getDeclaringType()).thenReturn(TenantAspectTest.class);

        when(joinPoint.getTarget()).thenReturn(new Object());

        try (MockedStatic<TenantContext> tctx = Mockito.mockStatic(TenantContext.class);
             MockedStatic<UserContext> uctx = Mockito.mockStatic(UserContext.class)) {
            tctx.when(TenantContext::getCurrentTenant).thenReturn(Integer.valueOf(77));

            UsersDto user = new UsersDto();
            user.setPermissions(Map.of()); // empty
            uctx.when(UserContext::getUser).thenReturn(user);

            when(commonUtils.getInterBranchContext()).thenReturn(null);
            tenantAspect.beforeFindOfMultiTenancyRepository(joinPoint);
            verify(entityManager, atLeastOnce()).unwrap(Session.class);
            verify(session, times(1)).enableFilter(MultiTenancy.TENANT_FILTER_NAME);
            verify(filter, times(1)).setParameter(eq(MultiTenancy.TENANT_PARAMETER_NAME), eq(77L));
        }
    }
}
