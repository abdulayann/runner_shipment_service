package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.utils.Generated;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.auth.AuthenticationException;

import javax.persistence.EntityNotFoundException;
import javax.persistence.PrePersist;
import javax.persistence.PreRemove;
import javax.persistence.PreUpdate;
import java.util.Map;
import java.util.Objects;

@Generated
@Slf4j
public class TenantEntityListener {

    public static final String AUTH_DENIED = "Authorization has been denied for this request, tenantId mismatch";

    @PrePersist
    public void prePersist(Object object) {
        if (object instanceof MultiTenancy multiTenancy) {
            log.info("PrePersist invoked for object: {}", object);

            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();
            var interBranchData = InterBranchContext.getContext();

            log.debug("Initial TenantId: {}, Permissions: {}, InterBranchContext: {}", tenantId, permissions, interBranchData);

            if ((permissions.containsKey(PermissionConstants.tenantSuperAdmin) || permissions.containsKey(PermissionConstants.crossTenantCreatePermission)) && isValidTenantId(tenantId)) {
                multiTenancy.setTenantId(tenantId);
                log.info("Setting TenantId from permissions: {}", tenantId);
            } else if (Objects.nonNull(interBranchData)
                    && (Boolean.TRUE.equals(interBranchData.isHub()) || Boolean.TRUE.equals(interBranchData.isCoLoadStation()))
                    && isValidTenantId(tenantId)) {
                multiTenancy.setTenantId(tenantId);
                log.info("Setting TenantId from InterBranchContext: {}", tenantId);
            } else {
                tenantId = TenantContext.getCurrentTenant();
                multiTenancy.setTenantId(tenantId);
                log.warn("Invalid TenantId. Defaulting to current tenant: {}", tenantId);
            }

            if (permissions.containsKey(PermissionConstants.tenantSuperAdmin) && UserContext.getUser().getSyncTenantId() != null) {
                Integer syncTenantId = UserContext.getUser().getSyncTenantId();
                multiTenancy.setTenantId(syncTenantId);
                log.info("Overriding TenantId for sync operation. New TenantId: {}", syncTenantId);
            }

            log.debug("Final TenantId after PrePersist: {}", multiTenancy.getTenantId());
        }
    }


    @PreUpdate
    public void preUpdate(Object object) throws AuthenticationException {
        if (object instanceof MultiTenancy multiTenancy) {
            log.info("PreUpdate invoked for object: {}", object);

            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();
            log.debug("Initial TenantId: {}, Permissions: {}", tenantId, permissions);

            if ((permissions.containsKey(PermissionConstants.tenantSuperAdmin) || permissions.containsKey(PermissionConstants.crossTenantUpdatePermission)) && isValidTenantId(tenantId)) {
                multiTenancy.setTenantId(tenantId);
                log.info("Updating TenantId based on permissions: {}", tenantId);
            } else if (!isValidTenantId(tenantId)) {
                tenantId = TenantContext.getCurrentTenant();
                multiTenancy.setTenantId(tenantId);
                log.warn("Invalid TenantId. Defaulting to current tenant: {}", tenantId);
            }

            InterBranchDto interBranchDto = InterBranchContext.getContext();
            log.debug("InterBranchContext during PreUpdate: {}", interBranchDto);

            if (Objects.nonNull(interBranchDto) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                if ((Boolean.TRUE.equals(interBranchDto.isHub()) && !interBranchDto.getColoadStationsTenantIds().contains(tenantId))
                        || (Boolean.TRUE.equals(interBranchDto.isCoLoadStation()) && !interBranchDto.getHubTenantIds().contains(tenantId))) {
                    log.error("TenantId validation failed for InterBranchContext. TenantId: {}", tenantId);
                    throw new AuthenticationException(AUTH_DENIED);
                }
            } else if (!permissions.containsKey(PermissionConstants.tenantSuperAdmin)
                    && !permissions.containsKey(PermissionConstants.crossTenantUpdatePermission)
                    && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                log.error("Unauthorized TenantId update attempt. TenantId: {}", tenantId);
                throw new AuthenticationException(AUTH_DENIED);
            }

            log.debug("Final TenantId after PreUpdate: {}", multiTenancy.getTenantId());
        }
    }

    @PreRemove
    public void preRemove(Object object) throws AuthenticationException {
        if (object instanceof MultiTenancy multiTenancy) {
            log.info("PreRemove invoked for object: {}", object);

            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();
            log.debug("Initial TenantId: {}, Permissions: {}", tenantId, permissions);

            if (permissions.containsKey(PermissionConstants.tenantSuperAdmin) && isValidTenantId(tenantId)) {
                multiTenancy.setTenantId(tenantId);
                log.info("Setting TenantId for super admin: {}", tenantId);
            } else if (Objects.isNull(tenantId)) {
                tenantId = TenantContext.getCurrentTenant();
                multiTenancy.setTenantId(tenantId);
                log.warn("TenantId is null. Defaulting to current tenant: {}", tenantId);
            }

            InterBranchDto interBranchDto = InterBranchContext.getContext();
            log.debug("InterBranchContext during PreRemove: {}", interBranchDto);

            if (Objects.nonNull(interBranchDto) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                if ((Boolean.TRUE.equals(interBranchDto.isHub()) && !interBranchDto.getColoadStationsTenantIds().contains(tenantId))
                        || (Boolean.TRUE.equals(interBranchDto.isCoLoadStation()) && !interBranchDto.getHubTenantIds().contains(tenantId))) {
                    log.error("TenantId validation failed for InterBranchContext. TenantId: {}", tenantId);
                    throw new AuthenticationException(AUTH_DENIED);
                }
            } else if (!permissions.containsKey(PermissionConstants.tenantSuperAdmin)
                    && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                log.error("Unauthorized TenantId removal attempt. TenantId: {}", tenantId);
                throw new EntityNotFoundException();
            }

            log.debug("Final TenantId after PreRemove: {}", multiTenancy.getTenantId());
        }
    }

    private boolean isValidTenantId(Integer tenantId) {
        boolean valid = (!Objects.isNull(tenantId) && tenantId > 0);
        log.debug("TenantId validation result for {}: {}", tenantId, valid);
        return valid;
    }
}

