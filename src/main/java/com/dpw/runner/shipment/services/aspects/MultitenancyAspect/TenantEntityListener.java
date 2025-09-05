package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.utils.Generated;
import java.util.Map;
import java.util.Objects;
import javax.persistence.EntityNotFoundException;
import javax.persistence.PrePersist;
import javax.persistence.PreRemove;
import javax.persistence.PreUpdate;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.auth.AuthenticationException;

@Generated
@Slf4j
public class TenantEntityListener {

    public static final String AUTH_DENIED = "Authorization has been denied for this request, tenantId mismatch";

    @PrePersist
    public void prePersist(Object object) {
        if (object instanceof MultiTenancy multiTenancy && !byPassMultiTenancyFilter(object)) {
            log.info("PrePersist invoked for object: {}", object);
            if (Boolean.TRUE.equals(IgnoreAutoTenantPopulationContext.getContext())) {
                log.info("Ignore the prePersist call for object: {}", object);
                return;
            }

            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();
            var interBranchData = InterBranchContext.getContext();

            if ((permissions.containsKey(PermissionConstants.TENANT_SUPER_ADMIN) || permissions.containsKey(PermissionConstants.CROSS_TENANT_CREATE_PERMISSION)) && isValidTenantId(tenantId)) {
                multiTenancy.setTenantId(tenantId);
            } else if (isInterBranchCase(interBranchData, tenantId)) {
                multiTenancy.setTenantId(tenantId);
            } else {
                tenantId = TenantContext.getCurrentTenant();
                multiTenancy.setTenantId(tenantId);
            }

            if (permissions.containsKey(PermissionConstants.TENANT_SUPER_ADMIN) && UserContext.getUser().getSyncTenantId() != null) {
                Integer syncTenantId = UserContext.getUser().getSyncTenantId();
                multiTenancy.setTenantId(syncTenantId);
            }

            log.info("Final TenantId after PrePersist: {}", multiTenancy.getTenantId());
        }
    }

    private boolean isInterBranchCase(InterBranchDto interBranchData, Integer tenantId) {
        return Objects.nonNull(interBranchData)
                && (Boolean.TRUE.equals(interBranchData.isHub()) || Boolean.TRUE.equals(interBranchData.isCoLoadStation()))
                && isValidTenantId(tenantId);
    }


    @PreUpdate
    public void preUpdate(Object object) throws AuthenticationException {
        if (object instanceof MultiTenancy multiTenancy && !byPassMultiTenancyFilter(object)) {
            log.info("PreUpdate invoked for object: {}", object);
            if (Boolean.TRUE.equals(IgnoreAutoTenantPopulationContext.getContext())) {
                log.info("Ignore the PreUpdate call for object: {}", object);
                return;
            }

            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();

            tenantId = getTenantInPreUpdate(multiTenancy, permissions, tenantId);

            InterBranchDto interBranchDto = InterBranchContext.getContext();

            if (Objects.nonNull(interBranchDto) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                if ((Boolean.TRUE.equals(interBranchDto.isHub()) && !interBranchDto.getColoadStationsTenantIds().contains(tenantId))
                        || (Boolean.TRUE.equals(interBranchDto.isCoLoadStation()) && !interBranchDto.getHubTenantIds().contains(tenantId))) {
                    throw new AuthenticationException(AUTH_DENIED);
                }
            } else if (!permissions.containsKey(PermissionConstants.TENANT_SUPER_ADMIN)
                    && !permissions.containsKey(PermissionConstants.CROSS_TENANT_UPDATE_PERMISSION)
                    && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                log.error("Unauthorized TenantId update attempt. Current Tenant Id: {} TenantId: {}", TenantContext.getCurrentTenant(), tenantId);
                throw new AuthenticationException(AUTH_DENIED);
            }

            log.info("Final TenantId after PreUpdate: {}", multiTenancy.getTenantId());
        }
    }

    private Integer getTenantInPreUpdate(MultiTenancy multiTenancy, Map<String, Boolean> permissions, Integer tenantId) {
        if ((permissions.containsKey(PermissionConstants.TENANT_SUPER_ADMIN) || permissions.containsKey(PermissionConstants.CROSS_TENANT_UPDATE_PERMISSION)) && isValidTenantId(tenantId)) {
            multiTenancy.setTenantId(tenantId);
        } else if (!isValidTenantId(tenantId)) {
            tenantId = TenantContext.getCurrentTenant();
            multiTenancy.setTenantId(tenantId);
        }
        return tenantId;
    }

    @PreRemove
    public void preRemove(Object object) throws AuthenticationException {
        if (object instanceof MultiTenancy multiTenancy && !byPassMultiTenancyFilter(object)) {
            log.info("PreRemove invoked for object: {}", object);
            if (Boolean.TRUE.equals(IgnoreAutoTenantPopulationContext.getContext())) {
                log.info("Ignore the preRemove call for object: {}", object);
                return;
            }

            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();

            tenantId = getTenantIdInPreRemove(multiTenancy, permissions, tenantId);

            InterBranchDto interBranchDto = InterBranchContext.getContext();

            if (Objects.nonNull(interBranchDto) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                if ((Boolean.TRUE.equals(interBranchDto.isHub()) && !interBranchDto.getColoadStationsTenantIds().contains(tenantId))
                        || (Boolean.TRUE.equals(interBranchDto.isCoLoadStation()) && !interBranchDto.getHubTenantIds().contains(tenantId))) {
                    throw new AuthenticationException(AUTH_DENIED);
                }
            } else if (!permissions.containsKey(PermissionConstants.TENANT_SUPER_ADMIN)
                    && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                log.error("Unauthorized TenantId update attempt. Current Tenant Id: {} TenantId: {}", TenantContext.getCurrentTenant(), tenantId);
                throw new EntityNotFoundException();
            }

            log.info("Final TenantId after PreRemove: {}", multiTenancy.getTenantId());
        }
    }

    private Integer getTenantIdInPreRemove(MultiTenancy multiTenancy, Map<String, Boolean> permissions, Integer tenantId) {
        if (permissions.containsKey(PermissionConstants.TENANT_SUPER_ADMIN) && isValidTenantId(tenantId)) {
            multiTenancy.setTenantId(tenantId);
        } else if (Objects.isNull(tenantId)) {
            tenantId = TenantContext.getCurrentTenant();
            multiTenancy.setTenantId(tenantId);
        }
        return tenantId;
    }

    private boolean isValidTenantId(Integer tenantId) {
        return (Objects.nonNull(tenantId) && tenantId > 0);
    }

    public Boolean byPassMultiTenancyFilter(Object object){
        if(object instanceof NetworkTransfer)
            return Boolean.TRUE;
        if(object instanceof Notification)
            return Boolean.TRUE;
        if(object instanceof Events)
            return Boolean.TRUE;
        return Boolean.FALSE;
    }
}

