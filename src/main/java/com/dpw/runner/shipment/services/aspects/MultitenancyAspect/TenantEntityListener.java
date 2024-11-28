package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.aspects.interbranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.entity.NetworkTransfer;
import com.dpw.runner.shipment.services.entity.Notification;
import com.dpw.runner.shipment.services.utils.Generated;
import org.apache.http.auth.AuthenticationException;
import javax.persistence.EntityNotFoundException;
import javax.persistence.PrePersist;
import javax.persistence.PreRemove;
import javax.persistence.PreUpdate;
import java.util.Map;
import java.util.Objects;

@Generated
public class TenantEntityListener {

    public static final String AUTH_DENIED = "Authorization has been denied for this request, tenantId mismatch";

    @PrePersist
    public void prePersist(Object object) {
        if (object instanceof MultiTenancy multiTenancy && !byPassMultiTenancyFilter(object)) {
            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();
            var interBranchData = InterBranchContext.getContext();

            if ((permissions.containsKey(PermissionConstants.tenantSuperAdmin) || permissions.containsKey(PermissionConstants.crossTenantCreatePermission)) && isValidTenantId(tenantId))
                multiTenancy.setTenantId(tenantId);
            else if (!Objects.isNull(interBranchData)
                    && (Boolean.TRUE.equals(interBranchData.isHub()) || Boolean.TRUE.equals(interBranchData.isCoLoadStation()))
                    && isValidTenantId(tenantId))
                multiTenancy.setTenantId(tenantId);
            else
                multiTenancy.setTenantId(TenantContext.getCurrentTenant());

            // Special case handled to retrigger sync from V1 to V2 on demand basis from admin account
            if (permissions.containsKey(PermissionConstants.tenantSuperAdmin) && !Objects.isNull(UserContext.getUser().getSyncTenantId()))
                multiTenancy.setTenantId(UserContext.getUser().getSyncTenantId());
        }
    }

    @PreUpdate
    public void preUpdate(Object object) throws AuthenticationException {
        if(object instanceof MultiTenancy multiTenancy && !byPassMultiTenancyFilter(object)) {
            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();

            if ((permissions.containsKey(PermissionConstants.tenantSuperAdmin) || permissions.containsKey(PermissionConstants.crossTenantUpdatePermission)) && isValidTenantId(tenantId))
                multiTenancy.setTenantId(tenantId);
            else if (!isValidTenantId(tenantId))
                multiTenancy.setTenantId(TenantContext.getCurrentTenant());

            if(!isValidTenantId(tenantId))
                tenantId = TenantContext.getCurrentTenant();

            InterBranchDto interBranchDto = InterBranchContext.getContext();
            if (Objects.nonNull(interBranchDto) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                if ((Boolean.TRUE.equals(interBranchDto.isHub()) && !interBranchDto.getColoadStationsTenantIds().contains(tenantId))
                        || (Boolean.TRUE.equals(interBranchDto.isCoLoadStation()) && !interBranchDto.getHubTenantIds().contains(tenantId)))
                    throw new AuthenticationException(AUTH_DENIED);
            }
            else if(! permissions.containsKey(PermissionConstants.tenantSuperAdmin) && !permissions.containsKey(PermissionConstants.crossTenantUpdatePermission) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId))
                throw new AuthenticationException(AUTH_DENIED);


        }
    }

    @PreRemove
    public void preRemove(Object object) throws AuthenticationException {
        if(object instanceof MultiTenancy multiTenancy && !byPassMultiTenancyFilter(object)) {
            Integer tenantId = multiTenancy.getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().getPermissions();

            if (permissions.containsKey(PermissionConstants.tenantSuperAdmin) && isValidTenantId(tenantId))
                multiTenancy.setTenantId(tenantId);
            else if (Objects.isNull(tenantId))
                multiTenancy.setTenantId(TenantContext.getCurrentTenant());

            InterBranchDto interBranchDto = InterBranchContext.getContext();
            if (Objects.nonNull(interBranchDto) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId)) {
                if ((Boolean.TRUE.equals(interBranchDto.isHub()) && !interBranchDto.getColoadStationsTenantIds().contains(tenantId))
                        || (Boolean.TRUE.equals(interBranchDto.isCoLoadStation()) && !interBranchDto.getHubTenantIds().contains(tenantId))) {
                    throw new AuthenticationException(AUTH_DENIED);
                }
            }

            else if(! permissions.containsKey(PermissionConstants.tenantSuperAdmin) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId))
                throw new EntityNotFoundException();
        }
    }

    private boolean isValidTenantId(Integer tenantId) {
        return (!Objects.isNull(tenantId) && tenantId > 0);
    }

    public Boolean byPassMultiTenancyFilter(Object object){
        if(object instanceof NetworkTransfer)
            return Boolean.TRUE;
        if(object instanceof Notification)
            return Boolean.TRUE;
        return Boolean.FALSE;
    }
}

